//! Contains zero pass builder of AVRA-rs
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::device::Device;
use crate::expr::Expr;
use crate::instruction::operation::Operation;
use crate::parser::{
    parse_iter, CodePoint, Item, Macro, ParseContext, ParseResult, Paths, Segment, SegmentType,
};

use failure::{bail, Error};
use maplit::btreeset;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass0 {
    // collect of segments
    pub segments: Vec<Segment>,
    // equals
    pub equs: HashMap<String, Expr>,
    // device
    pub device: Option<Device>,
}

impl BuildResultPass0 {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            equs: HashMap::new(),
            device: Some(Device::new(0)),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Pass0Context {
    pub current_path: PathBuf,
    pub include_paths: RefCell<Paths>,
    // equals
    pub equs: Rc<RefCell<HashMap<String, Expr>>>,
    // defines
    pub defines: Rc<RefCell<HashMap<String, Expr>>>,
    // device
    pub device: Rc<RefCell<Option<Device>>>,
    // segments
    pub segments: Rc<RefCell<Vec<Rc<RefCell<Segment>>>>>,
    // macro
    pub macros: Rc<Macro>,
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
        let equs = self.equs.borrow().clone();
        let device = self.device.borrow().clone();

        BuildResultPass0 {
            segments,
            equs,
            device,
        }
    }
}

pub fn build_pass_0(parsed: ParseResult) -> Result<BuildResultPass0, Error> {
    let context = Pass0Context {
        current_path: PathBuf::new(),
        include_paths: RefCell::new(btreeset! {}),
        equs: Rc::new(RefCell::new(parsed.equs)),
        defines: Rc::new(RefCell::new(parsed.defines)),
        device: Rc::new(RefCell::new(parsed.device)),
        segments: Rc::new(RefCell::new(vec![])),
        macros: Rc::new(Macro::new()),
    };

    for segment in parsed.segments {
        match segment.t {
            SegmentType::Data | SegmentType::Eeprom => {
                context.add_segment(segment.clone());
            }
            SegmentType::Code => {
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
    context.add_segment(Segment {
        address: segment.address,
        t: segment.t,
        items: vec![],
    });

    for (line, item) in segment.items.iter() {
        match item {
            Item::Instruction(name, _ops) => match name {
                Operation::Custom(macro_name) => {
                    if let Some(macro_body) = macroses.get(macro_name) {
                        let mut iter = macro_body.iter().map(|x| (x.0.line_num, x.1.as_str()));
                        let parse_context = ParseContext {
                            current_path: context.current_path.clone(),
                            include_paths: context.include_paths.clone(),
                            equs: context.equs.clone(),
                            defines: context.defines.clone(),
                            device: context.device.clone(),
                            segments: context.segments.clone(),
                            macros: context.macros.clone(),
                        };
                        parse_iter(&mut iter, &parse_context)?;
                    } else {
                        bail!("call undefined macro {} on {}", macro_name, line);
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

#[cfg(test)]
mod builder_tests {
    use super::*;
    use crate::instruction::{register::Reg8, InstructionOps};
    use crate::parser::parse_str;

    use maplit::hashmap;

    #[test]
    fn check_non_argument_macro() {
        let parse_result = parse_str("");
        let build_result = build_pass_0(parse_result.unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass0 {
                segments: vec![],
                equs: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(
            "
.macro test_one
        ldi r16, 1
        ldi r17, 2
.endm
        test_one
        test_one
        ",
        );
        let build_result = build_pass_0(parse_result.unwrap());
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
                equs: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }
}
