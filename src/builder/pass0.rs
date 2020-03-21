//! Contains zero pass builder of AVRA-rs
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::string::ToString;

use crate::context::CommonContext;
use crate::instruction::operation::Operation;
use crate::parser::{
    parse_iter, CodePoint, Item, Macro, ParseContext, ParseResult, Paths, Segment, SegmentType,
};

use crate::instruction::InstructionOps;
use failure::{bail, Error};
use maplit::btreeset;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass0 {
    // collect of segments
    pub segments: Vec<Segment>,
    // messages
    pub messages: Vec<String>,
}

impl BuildResultPass0 {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            messages: vec![],
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Pass0Context {
    pub current_path: PathBuf,
    pub include_paths: RefCell<Paths>,
    // common context
    pub common_context: CommonContext,
    // segments
    pub segments: Rc<RefCell<Vec<Rc<RefCell<Segment>>>>>,
    // macro
    pub macros: Rc<Macro>,
    // messages
    pub messages: Rc<RefCell<Vec<String>>>,
}

impl Pass0Context {
    pub fn add_segment(&self, segment: Segment) {
        self.segments
            .borrow_mut()
            .push(Rc::new(RefCell::new(segment)));
    }

    pub fn last_segment(&self) -> Option<Rc<RefCell<Segment>>> {
        if let Some(item) = self.segments.borrow().last() {
            Some(Rc::clone(item))
        } else {
            None
        }
    }

    pub fn push_to_last(&self, item: (CodePoint, Item)) {
        self.last_segment().unwrap().borrow_mut().items.push(item);
    }

    pub fn as_pass0_result(&self) -> BuildResultPass0 {
        let segments = self
            .segments
            .borrow()
            .iter()
            .filter(|x| !x.borrow().is_empty())
            .map(|x| x.borrow().clone())
            .collect();
        let messages = self.messages.borrow().clone();

        BuildResultPass0 { segments, messages }
    }
}

pub fn build_pass_0(
    parsed: ParseResult,
    common_context: &CommonContext,
) -> Result<BuildResultPass0, Error> {
    let context = Pass0Context {
        current_path: PathBuf::new(),
        include_paths: RefCell::new(btreeset! {}),
        common_context: common_context.clone(),
        segments: Rc::new(RefCell::new(vec![])),
        macros: Rc::new(Macro::new()),
        messages: Rc::new(RefCell::new(parsed.messages)),
    };

    for segment in parsed.segments {
        match segment.t {
            SegmentType::Data | SegmentType::Eeprom => {
                context.add_segment(segment.clone());
            }
            SegmentType::Code => {
                context.add_segment(Segment {
                    address: segment.address,
                    t: segment.t,
                    items: vec![],
                });
                pass0_internal(segment.clone(), &context, &parsed.macroses)?;
            }
        }
    }

    Ok(context.as_pass0_result())
}

fn pass0_internal(
    segment: Segment,
    context: &Pass0Context,
    macroses: &HashMap<String, Vec<(CodePoint, String)>>,
) -> Result<(), Error> {
    for (line, item) in segment.items.iter() {
        match item {
            Item::Instruction(name, ops) => match name {
                Operation::Custom(macro_name) => {
                    let segments = macro_expand(line, macro_name, ops, context, macroses)?;
                    if !segments.is_empty() {
                        let current_segment = context.last_segment().unwrap().borrow().clone();
                        if segments[0].address != current_segment.address
                            || segments[0].t != current_segment.t
                        {
                            context.add_segment(Segment {
                                address: segments[0].address,
                                t: segments[0].t,
                                items: vec![],
                            });
                        }
                        pass0_internal(segments[0].clone(), context, macroses)?;
                        for segment in segments.iter().skip(1) {
                            if segment.t == SegmentType::Code {
                                context.add_segment(Segment {
                                    address: segment.address,
                                    t: segment.t,
                                    items: vec![],
                                });
                                pass0_internal(segments[0].clone(), context, macroses)?;
                            } else {
                                context.add_segment(segment.clone());
                            }
                        }
                    }
                }
                _ => {
                    context.push_to_last((line.clone(), item.clone()));
                }
            },
            _ => {
                context.push_to_last((line.clone(), item.clone()));
            }
        }
    }

    Ok(())
}

fn macro_expand(
    line: &CodePoint,
    macro_name: &String,
    ops: &Vec<InstructionOps>,
    context: &Pass0Context,
    macroses: &HashMap<String, Vec<(CodePoint, String)>>,
) -> Result<Vec<Segment>, Error> {
    let segments = Rc::new(RefCell::new(vec![Rc::new(RefCell::new(Segment {
        items: vec![],
        t: SegmentType::Code,
        address: context.last_segment().unwrap().borrow().address,
    }))]));
    if let Some(macro_body) = macroses.get(macro_name) {
        let macro_body = if !ops.is_empty() {
            let mut processed = vec![];
            for (cp, raw_line) in macro_body {
                let mut raw_line = raw_line.clone();
                let string_rep = ops.iter().map(|x| x.to_string());
                for (num, replacer) in string_rep.enumerate() {
                    raw_line = raw_line.replace(&format!("@{}", num), replacer.as_str());
                }
                processed.push((cp.clone(), raw_line));
            }
            processed
        } else {
            macro_body.clone()
        };
        let mut iter = macro_body.iter().map(|x| (x.0.line_num, x.1.as_str()));
        let parse_context = ParseContext {
            current_path: context.current_path.clone(),
            include_paths: context.include_paths.clone(),
            common_context: context.common_context.clone(),
            segments: segments.clone(),
            macros: context.macros.clone(),
            messages: context.messages.clone(),
        };
        parse_iter(&mut iter, &parse_context)?;
    } else {
        bail!("call undefined macro {} on {}", macro_name, line);
    }

    let segments = segments
        .borrow()
        .iter()
        .filter(|x| !x.borrow().is_empty())
        .map(|x| x.borrow().clone())
        .collect();

    Ok(segments)
}

#[cfg(test)]
mod builder_tests {
    use super::*;
    use crate::expr::Expr;
    use crate::instruction::{register::Reg8, InstructionOps};
    use crate::parser::parse_str;

    #[test]
    fn check_non_argument_macro() {
        let common_context = CommonContext::new();
        let parse_result = parse_str("", &common_context);
        let build_result = build_pass_0(parse_result.unwrap(), &common_context);
        assert!(build_result.is_ok());

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
.macro test_one
        ldi r16, 1
        ldi r17, 2
.endm
        test_one
        test_one
        ",
            &common_context,
        );
        let build_result = build_pass_0(parse_result.unwrap(), &common_context);
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass0 {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(1)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(2)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(1)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(2)),
                                ]
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0x0,
                }],
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_for_argument_macro() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
.macro test_one
        ldi r16, @0
        ldi r17, @1
.endm
        test_one 1, 2
        test_one 3, 4
        ",
            &common_context,
        );
        let build_result = build_pass_0(parse_result.unwrap(), &common_context);
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass0 {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(1)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(2)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(3)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(4)),
                                ]
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0x0,
                }],
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_for_nested_macro() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
.macro test_one
        ldi r16, @0
        ldi r17, @1
.endm
.macro double_test_one
        test_one @0, @1
        test_one @2, @3
.endm
        double_test_one 1, 2, 3, 4
        ",
            &common_context,
        );
        let build_result = build_pass_0(parse_result.unwrap(), &common_context);
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass0 {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(1)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(2)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Const(3)),
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::E(Expr::Const(4)),
                                ]
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0x0,
                }],
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_for_conditional_macro() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
.macro test_one
    .ifdef last_one
    push r0
    .endif
    ret
.endm
    .define last_one
    test_one
        ",
            &common_context,
        );
        let build_result = build_pass_0(parse_result.unwrap(), &common_context);
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass0 {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(Operation::Push, vec![InstructionOps::R8(Reg8::R0),])
                        ),
                        (
                            CodePoint {
                                line_num: 6,
                                num: 2
                            },
                            Item::Instruction(Operation::Ret, vec![])
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0x0,
                }],
                messages: vec![],
            }
        );
    }
}
