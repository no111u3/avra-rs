//! Contains content parser of AVRA-rs

use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use crate::device::Device;
use crate::directive::{Directive, DirectiveOps};
use crate::document::{document, Document};
use crate::expr::Expr;
use crate::instruction::{operation::Operation, InstructionOps};

use failure::{bail, Error};
use maplit::btreeset;
use std::iter::Iterator;
use strum_macros::Display;

pub type Paths = BTreeSet<PathBuf>;

// TODO: add file name display support
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CodePoint {
    pub line_num: usize,
    pub num: usize,
}

impl fmt::Display for CodePoint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line: {}", self.line_num)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Item {
    Directive(Directive, DirectiveOps),
    Instruction(Operation, Vec<InstructionOps>),
    Label(String),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Display)]
#[strum(serialize_all = "lowercase")]
pub enum SegmentType {
    Code,
    Data,
    Eeprom,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Segment {
    pub items: Vec<(CodePoint, Item)>,
    pub t: SegmentType,
    /// start segment address
    pub address: u32,
}

impl Segment {
    pub fn new(t: SegmentType) -> Self {
        Self {
            items: vec![],
            t,
            address: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParseResult {
    // collect of segments
    pub segments: Vec<Segment>,
    // equals
    pub equs: HashMap<String, Expr>,
    // defines
    pub defines: HashMap<String, Expr>,
    // device
    pub device: Option<Device>,
}

impl ParseResult {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            equs: HashMap::new(),
            defines: HashMap::new(),
            device: Some(Device::new(0)),
        }
    }
}

pub fn parse_str(input: &str) -> Result<ParseResult, Error> {
    let mut result = ParseResult::new();

    let current_type = SegmentType::Code;

    let curr_segment = parse(input, &mut result, Segment::new(current_type), btreeset! {})?;

    if !curr_segment.is_empty() {
        result.segments.push(curr_segment);
    }

    Ok(result)
}

pub fn parse_file(path: PathBuf, paths: Paths) -> Result<ParseResult, Error> {
    let mut result = ParseResult::new();

    let current_type = SegmentType::Code;

    let curr_segment = parse_file_internal(path, &mut result, Segment::new(current_type), paths)?;

    if !curr_segment.is_empty() {
        result.segments.push(curr_segment);
    }

    Ok(result)
}

pub fn parse_file_internal(
    path: PathBuf,
    result: &mut ParseResult,
    curr_segment: Segment,
    paths: Paths,
) -> Result<Segment, Error> {
    let path = if !path.as_path().exists() {
        let mut new_path = PathBuf::new();
        for parent in paths.iter() {
            let mut full_path = parent.clone();
            full_path.push(path.clone());
            if full_path.as_path().exists() {
                new_path = full_path;
                break;
            }
        }

        if new_path == PathBuf::new() {
            path
        } else {
            new_path
        }
    } else {
        path
    };

    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => bail!(
            "Cannot read file {} because: {}",
            path.to_string_lossy(),
            err
        ),
    };

    let mut paths = paths.clone();
    if let Some(parent) = path.parent() {
        if let None = paths.get(parent) {
            paths.insert(parent.to_path_buf());
        }
    }

    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let curr_segment = parse(source.as_str(), result, curr_segment, paths)?;

    Ok(curr_segment)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NextItem {
    NewLine,
    EndIf,
    EndMacro,
    EndFile,
}

fn skip<'a>(
    iter: &mut dyn Iterator<Item = (usize, &'a str)>,
    ni: NextItem,
) -> Option<(usize, &'a str)> {
    let mut scoup_count = 0;
    match ni {
        NextItem::NewLine => iter.next(),
        NextItem::EndFile => None,
        other => {
            let mut ret = None;
            while let Some((_, line)) = iter.next() {
                if let Ok(item) = document::line(line) {
                    if let Document::DirectiveLine(_, directive, _) = item {
                        if other == NextItem::EndIf {
                            if directive == Directive::If
                                || directive == Directive::IfDef
                                || directive == Directive::IfNDef
                            {
                                scoup_count += 1;
                            }
                            else if directive == Directive::Endif || directive == Directive::Else {
                                if scoup_count == 0 {
                                    ret = iter.next();
                                    break;
                                } else {
                                    if directive == Directive::Endif {
                                        scoup_count -= 1;
                                    }
                                }
                            }
                        }
                        if other == NextItem::EndMacro && directive == Directive::EndMacro
                            || directive == Directive::EndM
                        {
                            ret = iter.next();
                            break;
                        }
                    }
                }
            }
            ret
        }
    }
}

pub fn parse(
    input: &str,
    result: &mut ParseResult,
    curr_segment: Segment,
    paths: Paths,
) -> Result<Segment, Error> {
    let mut curr_segment = curr_segment.clone();

    let mut lines = input.lines().enumerate();

    let mut next_item = NextItem::NewLine;

    loop {
        if let Some((line_num, line)) = skip(&mut lines, next_item) {
            next_item = NextItem::NewLine; // clear conditional flag to typical state
            let line_num = line_num + 1;
            let parsed_item = document::line(line);
            if let Ok(item) = parsed_item {
                match item {
                    Document::Label(name) => {
                        curr_segment
                            .items
                            .push((CodePoint { line_num, num: 1 }, Item::Label(name)));
                    }
                    Document::CodeLine(label, op, op_args) => {
                        if let Some(label) = *label {
                            if let Document::Label(name) = label {
                                curr_segment
                                    .items
                                    .push((CodePoint { line_num, num: 1 }, Item::Label(name)));
                            }
                        }
                        curr_segment.items.push((
                            CodePoint { line_num, num: 2 },
                            Item::Instruction(op, op_args),
                        ));
                    }
                    Document::DirectiveLine(label, d, d_op_args) => {
                        if let Some(label) = *label {
                            if let Document::Label(name) = label {
                                curr_segment
                                    .items
                                    .push((CodePoint { line_num, num: 1 }, Item::Label(name)));
                            }
                        }
                        let (item, segment) = d.parse(
                            &d_op_args,
                            result,
                            curr_segment,
                            paths.clone(),
                            CodePoint { line_num, num: 2 },
                        )?;
                        next_item = item;
                        curr_segment = segment;
                    }
                    Document::EmptyLine => {}
                    _ => {}
                }
            } else {
                bail!(
                    "failed to parse {} with error: {:?}",
                    CodePoint { line_num, num: 1 },
                    parsed_item
                );
            }
        } else {
            break;
        }
    }

    Ok(curr_segment)
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::expr::{BinaryExpr, BinaryOperator, UnaryExpr, UnaryOperator};
    use crate::instruction::{
        operation::{BranchT, SFlags},
        register::{Reg16, Reg8},
        IndexOps, InstructionOps,
    };

    use maplit::hashmap;

    #[test]
    fn check_empty() {
        let parse_result = parse_str("");

        assert_eq!(parse_result.is_ok(), true);
        assert_eq!(parse_result.unwrap(), ParseResult::new());

        let parse_result = parse_str("\t\r\n\t     \t\r\n\n");
        assert_eq!(parse_result.is_ok(), true);
        assert_eq!(parse_result.unwrap(), ParseResult::new());
    }

    #[test]
    fn check_wrong() {
        let parse_result = parse_str("bla bla bla bla");
        assert_eq!(parse_result.is_err(), true);
    }

    #[test]
    fn check_comment() {
        let parse_result = parse_str(";bla bla bla bla");
        assert_eq!(parse_result.unwrap(), ParseResult::new());

        let parse_result = parse_str(";;bla bla bla bla");
        assert_eq!(parse_result.unwrap(), ParseResult::new());
    }

    #[test]
    fn check_label() {
        let parse_result = parse_str("good_point:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 1,
                            num: 1
                        },
                        Item::Label("good_point".to_string())
                    )],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("good_point:\ngood_point2:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("good_point".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 1
                            },
                            Item::Label("good_point2".to_string())
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("good_point:; this is simple comment");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 1,
                            num: 1
                        },
                        Item::Label("good_point".to_string())
                    )],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("bad_point: bla bla bla");
        assert_eq!(parse_result.is_err(), true);

        let parse_result = parse_str("bad_point::");
        assert_eq!(parse_result.is_err(), true);

        let parse_result = parse_str("bad_point: bla bla bla");
        assert_eq!(parse_result.is_err(), true);

        let parse_result = parse_str("bad_point: bad_point2:");
        assert_eq!(parse_result.is_err(), true);
    }

    #[test]
    fn check_non_argument_commands() {
        let parse_result = parse_str("nop\nnop\nnop\nret");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(Operation::Ret, vec![])
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("label:\nnop\nnop\nret");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("label".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(Operation::Ret, vec![])
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("seh\nclh\nnop\ncli");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(Operation::Se(SFlags::H), vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(Operation::Cl(SFlags::H), vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(Operation::Nop, vec![])
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(Operation::Cl(SFlags::I), vec![])
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_one_argument_commands() {
        let parse_result = parse_str("push r0\nlsl r0\nswap r0\npop r1");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(Operation::Push, vec![InstructionOps::R8(Reg8::R0)])
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(Operation::Lsl, vec![InstructionOps::R8(Reg8::R0)])
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(Operation::Swap, vec![InstructionOps::R8(Reg8::R0)])
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(Operation::Pop, vec![InstructionOps::R8(Reg8::R1)])
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("tst r1\nbrpl exit\nrjmp error\n");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(Operation::Tst, vec![InstructionOps::R8(Reg8::R1)])
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Br(BranchT::Pl),
                                vec![InstructionOps::E(Expr::Ident("exit".to_string()))]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Rjmp,
                                vec![InstructionOps::E(Expr::Ident("error".to_string()))]
                            )
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_two_argument_commands() {
        let parse_result =
            parse_str("ldi r16, 1 << 2 | 1 << 1\nmov r0, r16\n subi r16, (-1)\nsts data, r16");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Binary(Box::new(BinaryExpr {
                                        left: Expr::Binary(Box::new(BinaryExpr {
                                            left: Expr::Const(1),
                                            operator: BinaryOperator::ShiftLeft,
                                            right: Expr::Const(2),
                                        })),
                                        operator: BinaryOperator::Or,
                                        right: Expr::Binary(Box::new(BinaryExpr {
                                            left: Expr::Const(1),
                                            operator: BinaryOperator::ShiftLeft,
                                            right: Expr::Const(1),
                                        })),
                                    })))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Mov,
                                vec![InstructionOps::R8(Reg8::R0), InstructionOps::R8(Reg8::R16)]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Subi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Unary(Box::new(UnaryExpr {
                                        operator: UnaryOperator::Minus,
                                        expr: Expr::Const(1)
                                    })))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Sts,
                                vec![
                                    InstructionOps::E(Expr::Ident("data".to_string())),
                                    InstructionOps::R8(Reg8::R16)
                                ]
                            )
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("ld r17, X\nld r18, Y+\nld r19, -Z\nst X+, r19");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ld,
                                vec![
                                    InstructionOps::R8(Reg8::R17),
                                    InstructionOps::Index(IndexOps::None(Reg16::X))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ld,
                                vec![
                                    InstructionOps::R8(Reg8::R18),
                                    InstructionOps::Index(IndexOps::PostIncrement(Reg16::Y))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ld,
                                vec![
                                    InstructionOps::R8(Reg8::R19),
                                    InstructionOps::Index(IndexOps::PreDecrement(Reg16::Z))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::St,
                                vec![
                                    InstructionOps::Index(IndexOps::PostIncrement(Reg16::X)),
                                    InstructionOps::R8(Reg8::R19)
                                ]
                            )
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("ldd r25, Z+2\nstd Z+6, r24");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 1,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldd,
                                vec![
                                    InstructionOps::R8(Reg8::R25),
                                    InstructionOps::Index(IndexOps::PostIncrementE(
                                        Reg16::Z,
                                        Expr::Const(2)
                                    ))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Std,
                                vec![
                                    InstructionOps::Index(IndexOps::PostIncrementE(
                                        Reg16::Z,
                                        Expr::Const(6)
                                    )),
                                    InstructionOps::R8(Reg8::R24)
                                ]
                            )
                        ),
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {},
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }
}
