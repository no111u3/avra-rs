//! Contains assembler directive representation of AVRA-rs

use strum_macros::{Display, EnumString};

use crate::device::{Device, DEVICES};
use crate::expr::Expr;
use crate::parser::{
    parse_file_internal, CodePoint, Item, NextItem, ParseContext, ParseResult, Paths, Segment,
    SegmentType,
};

use crate::context::Context;
use failure::{bail, Error};
use std::path::PathBuf;

#[derive(Clone, PartialEq, Eq, Debug, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Directive {
    /// Reserve bytes for a variable in RAM
    Byte,
    /// Code segment
    CSeg,
    /// Program memory size for AT94K (unsupported)
    CSegSize,
    /// Define constant bytes in program memory and EEPROM
    Db,
    /// Define a symbolic name on a register
    Def,
    /// Device specific
    Device,
    /// Data segment
    DSeg,
    /// Define constant words in program memory and EEPROM
    Dw,
    /// End of macro define (unsupported)
    EndM,
    EndMacro,
    /// Set a symbol equal to an expression
    Equ,
    /// EEPROM segment
    ESeg,
    /// Exit from file
    Exit,
    /// Read source from another file
    Include,
    /// Another path for includes
    IncludePath,
    /// Turn listfile generation on (unsupported)
    List,
    /// Turn macro expansion in list file on (unsupported)
    ListMac,
    /// Begin macro (unsupported)
    Macro,
    /// Turn listfile generation off (unsupported)
    NoList,
    /// Set program origin
    Org,
    /// Set a symbol to an expression
    Set,
    /// Define a preprocessor macro
    Define,
    /// Conditional assembly - alternate branches (partially)
    Else,
    ElIf,
    /// Conditional assembly - end conditional block
    Endif,
    /// Outputs an error message (unsupported)
    Error,
    /// Conditional assembly - begin of conditional block (partially)
    If,
    IfDef,
    IfNDef,
    /// Outputs a message string (unsupported)
    Message,
    /// Define constant double words in program memory and EEPROM (unsupported)
    Dd,
    /// Define constant quad words in program memory and EEPROM (unsupported)
    Dq,
    /// Undefine register symbol
    Undef,
    /// Outputs a warning message (unsupported)
    Warning,
    /// Set up/down overlapping section (unsupported)
    Overlap,
    NoOverlap,
    /// Preprocessor pragmas (partially)
    Pragma,

    /// For extend and macross support
    #[strum(disabled = "true")]
    Custom(String),
}

impl Directive {
    pub fn parse(
        &self,
        opts: &DirectiveOps,
        result: &mut ParseResult,
        curr_segment: Segment,
        current_path: &PathBuf,
        paths: &mut Paths,
        point: CodePoint,
    ) -> Result<(NextItem, Segment), Error> {
        let mut curr_segment = curr_segment.clone();
        let mut current_type = curr_segment.t;
        let mut next_item = NextItem::NewLine;

        match self {
            Directive::Db
            | Directive::Dw
            | Directive::Set
            | Directive::Def
            | Directive::Undef
            | Directive::Pragma
            | Directive::Byte => {
                curr_segment
                    .items
                    .push((point, Item::Directive(self.clone(), opts.clone())));
            }
            Directive::Equ => {
                if let DirectiveOps::Assign(name, value) = opts {
                    if let Expr::Ident(name) = name {
                        result.equs.insert(name.clone(), value.clone());
                    }
                } else {
                    bail!("wrong format for .equ, expected: {} in {}", opts, point,);
                }
            }
            Directive::Org => {
                if let DirectiveOps::OpList(values) = opts {
                    if let Operand::E(Expr::Const(value)) = &values[0] {
                        if !curr_segment.is_empty() {
                            result.segments.push(curr_segment);

                            curr_segment = Segment::new(current_type);
                        }
                        curr_segment.address = *value as u32;
                    }
                } else {
                    bail!("wrong format for .org, expected: {} in {}", opts, point,);
                }
            }
            Directive::CSeg | Directive::DSeg | Directive::ESeg => {
                current_type = match self {
                    Directive::CSeg => SegmentType::Code,
                    Directive::DSeg => SegmentType::Data,
                    Directive::ESeg => SegmentType::Eeprom,
                    _ => SegmentType::Code,
                };

                if !curr_segment.is_empty() {
                    result.segments.push(curr_segment);

                    curr_segment = Segment::new(current_type);
                } else {
                    curr_segment.t = current_type;
                }
            }
            Directive::Device => {
                if let DirectiveOps::OpList(values) = opts {
                    if let Operand::E(Expr::Ident(value)) = &values[0] {
                        if let Some(device) = DEVICES.get(value.as_str()) {
                            if let Some(old_device) = &result.device {
                                if old_device == &Device::new(0) {
                                    result.device = Some(device.clone());
                                } else {
                                    bail!(
                                        "device redefinition in {}, old: {:?} -> new: {:?}",
                                        point,
                                        old_device,
                                        device,
                                    )
                                }
                            } else {
                                result.device = Some(device.clone());
                            }
                        } else {
                            bail!("unknown device {} in {}", value, point,)
                        }
                    }
                } else {
                    bail!("wrong format for .device, expected: {} in {}", opts, point,);
                }
            }
            Directive::Include => {
                if let DirectiveOps::OpList(values) = &opts {
                    if let Operand::S(include) = &values[0] {
                        let context = ParseContext {
                            current_path: PathBuf::from(include),
                            include_paths: paths.clone(),
                        };
                        curr_segment = parse_file_internal(result, curr_segment, context)?;
                    } else {
                        bail!("wrong format for .include, expected: {} in {}", opts, point,);
                    }
                } else {
                    bail!("wrong format for .include, expected: {} in {}", opts, point,);
                }
            }
            Directive::IncludePath => {
                if let DirectiveOps::OpList(values) = &opts {
                    if let Operand::S(include) = &values[0] {
                        let path = PathBuf::from(include);
                        let path = if path.is_relative() {
                            let mut current_path = current_path.parent().unwrap().to_path_buf();
                            current_path.push(path);
                            current_path
                        } else {
                            path
                        };
                        if !paths.contains(&path) {
                            paths.insert(path);
                        }
                    } else {
                        bail!(
                            "wrong format for .includepath, expected: {} in {}",
                            opts,
                            point,
                        );
                    }
                } else {
                    bail!(
                        "wrong format for .includepath, expected: {} in {}",
                        opts,
                        point,
                    );
                }
            }
            Directive::IfNDef | Directive::IfDef => {
                if let DirectiveOps::OpList(values) = &opts {
                    if let Operand::E(Expr::Ident(name)) = &values[0] {
                        if result.defines.contains_key(name) {
                            if self == &Directive::IfNDef {
                                next_item = NextItem::EndIf;
                            };
                        } else {
                            if self == &Directive::IfDef {
                                next_item = NextItem::EndIf;
                            }
                        }
                    } else {
                        bail!(
                            "wrong format for .{}, expected: {} in {}",
                            self,
                            opts,
                            point,
                        );
                    }
                } else {
                    bail!(
                        "wrong format for .{}, expected: {} in {}",
                        self,
                        opts,
                        point,
                    );
                }
            }
            Directive::Define => {
                if let DirectiveOps::OpList(values) = &opts {
                    if let Operand::E(Expr::Ident(name)) = &values[0] {
                        result.defines.insert(name.clone(), Expr::Const(0));
                    } else {
                        bail!("wrong format for .define, expected: {} in {}", opts, point,);
                    }
                } else {
                    bail!("wrong format for .define, expected: {} in {}", opts, point,);
                }
            }
            Directive::Else => {
                next_item = NextItem::EndIf;
            }
            Directive::Endif => {}
            Directive::Exit => {
                next_item = NextItem::EndFile;
            }
            Directive::Custom(name) => bail!("unsupported custom directive {}, {}", name, point),
            _ => bail!(
                "Unsupported directive {} in {} segment, {}",
                self,
                current_type,
                point,
            ),
        }

        Ok((next_item, curr_segment))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operand {
    E(Expr),
    S(String),
}

impl Operand {
    pub fn len(&self) -> usize {
        match self {
            Operand::E(_) => 1,
            Operand::S(s) => s.len(),
        }
    }

    pub fn get_bytes(&self, e_p: &dyn Context) -> Result<Vec<u8>, Error> {
        match self {
            Operand::E(expr) => Ok(vec![expr.get_byte(e_p)?]),
            Operand::S(s) => Ok(s.as_bytes().to_vec()),
        }
    }

    pub fn get_words(&self, e_p: &dyn Context) -> Result<Vec<u8>, Error> {
        match self {
            Operand::E(expr) => Ok(expr.get_words(e_p)?.to_vec()),
            Operand::S(_) => bail!("not allowed to convert string as 2 bytes array"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Display)]
pub enum DirectiveOps {
    OpList(Vec<Operand>),
    Assign(Expr, Expr),
}

impl DirectiveOps {
    pub fn len(&self) -> usize {
        match self {
            DirectiveOps::OpList(items) => items.iter().fold(0, |acc, op| acc + op.len()),
            DirectiveOps::Assign(_, _) => 1,
        }
    }

    pub fn get_bytes(&self, constants: &dyn Context) -> Result<Vec<u8>, Error> {
        match self {
            DirectiveOps::OpList(items) => {
                let mut bytes = vec![];
                for item in items {
                    bytes.extend(item.get_bytes(constants)?);
                }

                Ok(bytes)
            }
            DirectiveOps::Assign(_, _) => bail!("Cannot convert assignment to bytes"),
        }
    }

    pub fn get_words(&self, constants: &dyn Context) -> Result<Vec<u8>, Error> {
        match self {
            DirectiveOps::OpList(items) => {
                let mut bytes = vec![];
                for item in items {
                    bytes.extend(item.get_words(constants)?);
                }

                Ok(bytes)
            }
            DirectiveOps::Assign(_, _) => bail!("Cannot convert assignment to words"),
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use std::collections::HashMap;

    use crate::device::DEVICES;
    use crate::directive::Operand;
    use crate::document::{document, Document};
    use crate::expr::{BinaryExpr, BinaryOperator};
    use crate::instruction::{
        operation::Operation,
        register::{Reg16, Reg8},
        IndexOps, InstructionOps,
    };
    use crate::parser::{parse_file, parse_str};

    use maplit::{btreeset, hashmap};
    use std::path::PathBuf;

    #[test]
    fn directive_test() {
        assert_eq!(document::directive(".equ"), Ok(Directive::Equ));

        assert_eq!(document::directive(".dseg"), Ok(Directive::DSeg));
    }

    #[test]
    fn directive_op_test() {
        assert_eq!(
            document::directive_op("a"),
            Ok(Operand::E(Expr::Ident("a".to_string())))
        );

        assert_eq!(
            document::directive_op("\"bla bla bla\""),
            Ok(Operand::S("bla bla bla".to_string()))
        );
    }

    #[test]
    fn directive_ops_test() {
        assert_eq!(
            document::directive_ops(""),
            Ok(DirectiveOps::OpList(vec![]))
        );

        assert_eq!(
            document::directive_ops("a\t, b,c  ,\td"),
            Ok(DirectiveOps::OpList(vec![
                Operand::E(Expr::Ident("a".to_string())),
                Operand::E(Expr::Ident("b".to_string())),
                Operand::E(Expr::Ident("c".to_string())),
                Operand::E(Expr::Ident("d".to_string()))
            ]))
        );

        assert_eq!(
            document::directive_ops("a,b,c,d"),
            Ok(DirectiveOps::OpList(vec![
                Operand::E(Expr::Ident("a".to_string())),
                Operand::E(Expr::Ident("b".to_string())),
                Operand::E(Expr::Ident("c".to_string())),
                Operand::E(Expr::Ident("d".to_string()))
            ]))
        );

        assert_eq!(
            document::directive_ops("a b c d"),
            Ok(DirectiveOps::OpList(vec![
                Operand::E(Expr::Ident("a".to_string())),
                Operand::E(Expr::Ident("b".to_string())),
                Operand::E(Expr::Ident("c".to_string())),
                Operand::E(Expr::Ident("d".to_string()))
            ]))
        );

        assert_eq!(
            document::directive_ops("t = 44"),
            Ok(DirectiveOps::Assign(
                Expr::Ident("t".to_string()),
                Expr::Const(44)
            ))
        );
    }

    #[test]
    fn directive_line_test() {
        assert_eq!(
            document::directive_line(".dseg"),
            Ok(Document::DirectiveLine(
                Box::new(None),
                Directive::DSeg,
                DirectiveOps::OpList(vec![])
            ))
        );

        assert_eq!(
            document::directive_line(".equ Last = 8"),
            Ok(Document::DirectiveLine(
                Box::new(None),
                Directive::Equ,
                DirectiveOps::Assign(Expr::Ident("Last".to_string()), Expr::Const(8))
            ))
        );

        assert_eq!(
            document::directive_line("#define MIDDLE"),
            Ok(Document::DirectiveLine(
                Box::new(None),
                Directive::Define,
                DirectiveOps::OpList(vec![Operand::E(Expr::Ident("MIDDLE".to_string()))])
            ))
        );
    }

    #[test]
    fn check_org() {
        let parse_result = parse_str("good_point:\n.org 0x2\ngood_point2:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("good_point".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 3,
                                num: 1
                            },
                            Item::Label("good_point2".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0x2
                    }
                ],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_segments() {
        let parse_result = parse_str("good_point:\n.cseg\ngood_point2:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("good_point".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 3,
                                num: 1
                            },
                            Item::Label("good_point2".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    }
                ],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("good_point:\n.cseg\n.org 0x20\ngood_point2:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("good_point".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 4,
                                num: 1
                            },
                            Item::Label("good_point2".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0x20
                    }
                ],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("good_point:\n.dseg\ngood_point2:\n.cseg\ngood_point3:");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 1,
                                num: 1
                            },
                            Item::Label("good_point".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 3,
                                num: 1
                            },
                            Item::Label("good_point2".to_string())
                        )],
                        t: SegmentType::Data,
                        address: 0
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 5,
                                num: 1
                            },
                            Item::Label("good_point3".to_string())
                        )],
                        t: SegmentType::Code,
                        address: 0
                    },
                ],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_db_dw() {
        let parse_result = parse_str("ldi r16, data\ndata:\n.db 15, 26, \"Hello, World\", end");
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
                                    InstructionOps::E(Expr::Ident("data".to_string()))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 1
                            },
                            Item::Label("data".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Db,
                                DirectiveOps::OpList(vec![
                                    Operand::E(Expr::Const(15)),
                                    Operand::E(Expr::Const(26)),
                                    Operand::S("Hello, World".to_string()),
                                    Operand::E(Expr::Ident("end".to_string())),
                                ])
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("ldi r18, data_w\ndata_w:\n.dw 0xff44, end, 0xda4e");
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
                                    InstructionOps::R8(Reg8::R18),
                                    InstructionOps::E(Expr::Ident("data_w".to_string()))
                                ]
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 1
                            },
                            Item::Label("data_w".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Dw,
                                DirectiveOps::OpList(vec![
                                    Operand::E(Expr::Const(0xff44)),
                                    Operand::E(Expr::Ident("end".to_string())),
                                    Operand::E(Expr::Const(0xda4e))
                                ])
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_directive_equ() {
        let parse_result = parse_str(".equ REG0 = 0x44\n.equ REG1 = 0x45\n.equ REG2 = 0x46");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: hashmap! {
                    "REG0".to_string() => Expr::Const(0x44),
                    "REG1".to_string() => Expr::Const(0x45),
                    "REG2".to_string() => Expr::Const(0x46),
                },
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_directive_set() {
        let parse_result = parse_str(".set t = 4\n.set t = t + 1");
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
                            Item::Directive(
                                Directive::Set,
                                DirectiveOps::Assign(Expr::Ident("t".to_string()), Expr::Const(4))
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Set,
                                DirectiveOps::Assign(
                                    Expr::Ident("t".to_string()),
                                    Expr::Binary(Box::new(BinaryExpr {
                                        left: Expr::Ident("t".to_string()),
                                        operator: BinaryOperator::Add,
                                        right: Expr::Const(1)
                                    }))
                                )
                            )
                        )
                    ],
                    t: SegmentType::Code,
                    address: 0,
                }],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_directive_byte() {
        let parse_result = parse_str(".dseg\ndata: .byte 1\ncounter: .byte 2");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 2,
                                num: 1
                            },
                            Item::Label("data".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Byte,
                                DirectiveOps::OpList(vec![Operand::E(Expr::Const(1))])
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 1
                            },
                            Item::Label("counter".to_string())
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Byte,
                                DirectiveOps::OpList(vec![Operand::E(Expr::Const(2))])
                            )
                        ),
                    ],
                    t: SegmentType::Data,
                    address: 0,
                }],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_directive_device() {
        let parse_result = parse_str(".device ATmega48\nldd r25, Z+2");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 2,
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
                    ),],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(DEVICES.get("ATmega48").unwrap().clone()),
            }
        );

        let parse_result = parse_str(".device ATmega96\nldd r25, Z+2");
        assert!(parse_result.is_err());
    }

    #[test]
    fn check_directive_include() {
        let parse_result = parse_file(PathBuf::from("tests/include_test.asm"), btreeset! {});

        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 3,
                            num: 2
                        },
                        Item::Instruction(
                            Operation::In,
                            vec![
                                InstructionOps::R8(Reg8::R0),
                                InstructionOps::E(Expr::Ident("SREG".to_string()))
                            ]
                        )
                    ),],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {
                    "SREG".to_string() => Expr::Const(0x3f),
                },
                defines: hashmap! {},
                device: Some(DEVICES.get("ATmega48").unwrap().clone()),
            }
        );
    }

    #[test]
    fn check_directive_include_path() {
        let parse_result = parse_file(PathBuf::from("tests/include_path_test.asm"), btreeset! {});

        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 4,
                            num: 2
                        },
                        Item::Instruction(
                            Operation::In,
                            vec![
                                InstructionOps::R8(Reg8::R0),
                                InstructionOps::E(Expr::Ident("SREG".to_string()))
                            ]
                        )
                    ),],
                    t: SegmentType::Code,
                    address: 0
                }],
                equs: hashmap! {
                    "SREG".to_string() => Expr::Const(0x3f),
                },
                defines: hashmap! {},
                device: Some(DEVICES.get("ATmega88").unwrap().clone()),
            }
        );
    }

    #[test]
    fn check_directive_ifdef_ifndef_else_define() {
        let parse_result = parse_str(".define T\n.ifndef T\n.endif");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "T".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("\n.ifndef T\n.endif");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(".ifdef T\n.endif");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! {},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(".define T\n.ifdef T\n.endif");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "T".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str("\n.ifndef T\n.ifdef T\n.define T\n.endif\n.define X\n.endif");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "X".to_string() => Expr::Const(0)},
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(".ifdef T\n.define X\n.else\n.define Y\n.endif\n.define Z");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "Y".to_string() => Expr::Const(0), "Z".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(".ifndef T\n.define X\n.else\n.define Y\n.endif\n.define Z");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "X".to_string() => Expr::Const(0), "Z".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );

        let parse_result = parse_str(".ifndef T\n.define X\n.else\n.endif\n.define Z");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "X".to_string() => Expr::Const(0), "Z".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );
    }

    #[test]
    fn check_directive_exit() {
        let parse_result = parse_str(".define Y\n.exit\n.define X");
        assert_eq!(
            parse_result.unwrap(),
            ParseResult {
                segments: vec![],
                equs: HashMap::new(),
                defines: hashmap! { "Y".to_string() => Expr::Const(0) },
                device: Some(Device::new(0)),
            }
        );
    }
}
