//! Contains first pass builder of AVRA-rs

use std::collections::HashMap;

use crate::builder::pass0::BuildResultPass0;
use crate::device::Device;
use crate::directive::{Directive, DirectiveOps, Operand};
use crate::expr::Expr;
use crate::parser::{CodePoint, Item, ParseResult, Segment, SegmentType};

use failure::{bail, Error};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass1 {
    // Input segments
    pub segments: Vec<Segment>,
    // equals
    pub equs: HashMap<String, Expr>,
    // labels
    pub labels: HashMap<String, (SegmentType, u32)>,
    // device
    pub device: Device,
}

pub fn build_pass_1(parsed: BuildResultPass0) -> Result<BuildResultPass1, Error> {
    let device = parsed.device.unwrap_or(Device::new(0));
    let mut segments = vec![];
    let mut labels = HashMap::new();
    let mut code_offset = 0;
    let mut data_offset = device.ram_start;
    let mut eeprom_offset = 0;
    for segment in parsed.segments {
        let offset = match segment.t {
            SegmentType::Code => code_offset,
            SegmentType::Data => data_offset,
            SegmentType::Eeprom => eeprom_offset,
        };

        let (current_end_offset, current_offset, items) =
            pass_1_internal(&segment, offset, &mut labels)?;
        segments.push(Segment {
            items,
            t: segment.t,
            address: current_offset,
        });

        match segment.t {
            SegmentType::Code => {
                code_offset = current_end_offset;
            }
            SegmentType::Data => {
                data_offset = current_end_offset;
            }
            SegmentType::Eeprom => {
                eeprom_offset = current_end_offset;
            }
        }
    }

    Ok(BuildResultPass1 {
        segments,
        equs: parsed.equs,
        labels,
        device,
    })
}

fn pass_1_internal(
    segment: &Segment,
    address: u32,
    labels: &mut HashMap<String, (SegmentType, u32)>,
) -> Result<(u32, u32, Vec<(CodePoint, Item)>), Error> {
    let current_offset = if segment.address == 0 {
        address
    } else {
        if segment.address < address {
            bail!("segment overlapping isn't supported");
        }
        segment.address
    };

    let mut out_items = vec![];
    let mut cur_address = current_offset;

    for (line, item) in &segment.items {
        match item {
            Item::Label(name) => {
                if let Some(_) = labels.insert(name.clone(), (segment.t, cur_address)) {
                    // TODO: add display current string of mistake and previous location
                    bail!("Identifier {} is used twice, {}", name, line);
                }
            }
            another_item => {
                if let Item::Instruction(op, _) = &another_item {
                    match segment.t {
                        SegmentType::Code => {
                            cur_address += op.info().len;
                            out_items.push((*line, another_item.clone()));
                        }
                        _ => bail!(
                            "instructions are not allowed in {} segment, {}",
                            segment.t,
                            line
                        ),
                    }
                } else if let Item::Directive(d, d_op) = &another_item {
                    match d {
                        Directive::Set | Directive::Def | Directive::Undef => {
                            out_items.push((*line, another_item.clone()));
                        }
                        Directive::Db => {
                            if let DirectiveOps::OpList(args) = d_op {
                                let mut args = args.clone();

                                cur_address += match segment.t {
                                    SegmentType::Code => {
                                        (if d_op.len() % 2 == 1 {
                                            args.push(Operand::E(Expr::Const(0x0)));
                                            d_op.len() + 1
                                        } else {
                                            d_op.len()
                                        }) as u32
                                            / 2
                                    }
                                    SegmentType::Eeprom => d_op.len() as u32,
                                    _ => bail!(".db are not allowed in data segment, {}", line),
                                };

                                out_items.push((
                                    *line,
                                    Item::Directive(Directive::Db, DirectiveOps::OpList(args)),
                                ));
                            } else {
                                bail!("Not allowed type of arguments for .db, {}", line);
                            }
                        }
                        Directive::Dw => {
                            if let DirectiveOps::OpList(args) = d_op {
                                cur_address += match segment.t {
                                    SegmentType::Code => d_op.len() as u32,
                                    SegmentType::Eeprom => d_op.len() as u32 * 2,
                                    _ => bail!(".dw are not allowed in data segment, {}", line),
                                };

                                out_items.push((
                                    *line,
                                    Item::Directive(
                                        Directive::Dw,
                                        DirectiveOps::OpList(args.clone()),
                                    ),
                                ));
                            } else {
                                bail!("Not allowed type of arguments for .db, {}", line);
                            }
                        }
                        Directive::Byte => match segment.t {
                            SegmentType::Data => {
                                if let DirectiveOps::OpList(args) = d_op {
                                    if args.len() > 1 {
                                        bail!("Too many arguments for {}", d);
                                    }
                                    if let Operand::E(expr) = &args[0] {
                                        if let Expr::Const(n) = expr {
                                            cur_address += *n as u32;
                                        }
                                    }
                                } else {
                                    bail!("Not allowed type of arguments for .byte, {}", line);
                                }
                            }
                            _ => bail!(".byte are not allowed in {} segment, {}", segment.t, line),
                        },
                        Directive::Pragma => {}
                        _ => bail!("unsupported directive {}, {}", d, line),
                    }
                }
            }
        }
    }

    Ok((cur_address, current_offset, out_items))
}

#[cfg(test)]
mod builder_tests {
    use super::*;
    use crate::builder::pass0::build_pass_0;
    use crate::device::Device;
    use crate::directive::Operand;
    use crate::instruction::{operation::Operation, register::Reg8, InstructionOps};
    use crate::parser::parse_str;
    use maplit::hashmap;

    #[test]
    fn check_empty() {
        let build_result = build_pass_1(BuildResultPass0::new());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![],
                equs: HashMap::new(),
                labels: HashMap::new(),
                device: Device::new(0)
            }
        );
    }

    #[test]
    fn check_labels() {
        let build_result = build_pass_1(BuildResultPass0 {
            segments: vec![Segment {
                items: vec![(
                    CodePoint {
                        line_num: 1,
                        num: 1,
                    },
                    Item::Label("good_point".to_string()),
                )],
                t: SegmentType::Code,
                address: 0,
            }],
            equs: HashMap::new(),
            device: Some(Device::new(0)),
        });

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![Segment {
                    items: vec![],
                    t: SegmentType::Code,
                    address: 0,
                }],
                equs: HashMap::new(),
                labels: hashmap! {"good_point".to_string() => (SegmentType::Code, 0)},
                device: Device::new(0)
            }
        );
    }

    #[test]
    fn check_segments() {
        let build_result = build_pass_1(BuildResultPass0 {
            segments: vec![
                Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 1,
                            num: 1,
                        },
                        Item::Label("good_point".to_string()),
                    )],
                    t: SegmentType::Code,
                    address: 0,
                },
                Segment {
                    items: vec![(
                        CodePoint {
                            line_num: 2,
                            num: 1,
                        },
                        Item::Label("good_point2".to_string()),
                    )],
                    t: SegmentType::Code,
                    address: 0x2,
                },
            ],
            equs: HashMap::new(),
            device: Some(Device::new(0)),
        });

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0,
                    },
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0x2,
                    }
                ],
                equs: HashMap::new(),
                labels: hashmap! {
                    "good_point".to_string() => (SegmentType::Code, 0),
                    "good_point2".to_string() => (SegmentType::Code, 0x2),

                },
                device: Device::new(0)
            }
        );

        let parse_result = parse_str("good_point:\n.cseg\n.org 0x20\ngood_point2:");

        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_1(post_parse_result.unwrap());

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0,
                    },
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0x20,
                    }
                ],
                equs: HashMap::new(),
                labels: hashmap! {
                    "good_point".to_string() => (SegmentType::Code, 0),
                    "good_point2".to_string() => (SegmentType::Code, 0x20),
                },
                device: Device::new(0)
            }
        );

        let parse_result = parse_str("good_point:\n.dseg\ngood_point2:\n.cseg\ngood_point3:");

        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_1(post_parse_result.unwrap());

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0,
                    },
                    Segment {
                        items: vec![],
                        t: SegmentType::Data,
                        address: 0x60,
                    },
                    Segment {
                        items: vec![],
                        t: SegmentType::Code,
                        address: 0,
                    }
                ],
                equs: HashMap::new(),
                labels: hashmap! {
                    "good_point".to_string() => (SegmentType::Code, 0),
                    "good_point2".to_string() => (SegmentType::Data, 0x60),
                    "good_point3".to_string() => (SegmentType::Code, 0),
                },
                device: Device::new(0)
            }
        );
    }

    #[test]
    fn check_data_placement() {
        let parse_result = parse_str(
            "
        ldi r16, data
        rjmp m1
data:   .db 15, 26, \"Hello, World\", end
data_w:
        .dw 0xff44, end, 0xda4e
m1:
        ldi r18, data_w
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_1(post_parse_result.unwrap());

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![Segment {
                    items: vec![
                        (
                            CodePoint {
                                line_num: 2,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R16),
                                    InstructionOps::E(Expr::Ident("data".to_string())),
                                ],
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 3,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Rjmp,
                                vec![InstructionOps::E(Expr::Ident("m1".to_string()))],
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 4,
                                num: 2
                            },
                            Item::Directive(
                                Directive::Db,
                                DirectiveOps::OpList(vec![
                                    Operand::E(Expr::Const(15)),
                                    Operand::E(Expr::Const(26)),
                                    Operand::S("Hello, World".to_string()),
                                    Operand::E(Expr::Ident("end".to_string())),
                                    Operand::E(Expr::Const(0)),
                                ]),
                            )
                        ),
                        (
                            CodePoint {
                                line_num: 6,
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
                        ),
                        (
                            CodePoint {
                                line_num: 8,
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
                    ],
                    t: SegmentType::Code,
                    address: 0,
                }],
                equs: HashMap::new(),
                labels: hashmap! {
                    "data".to_string() => (SegmentType::Code, 2),
                    "data_w".to_string() => (SegmentType::Code, 10),
                    "m1".to_string() => (SegmentType::Code, 13),
                },
                device: Device::new(0)
            }
        );

        let parse_result = parse_str(
            "
        ldi r16, data
        rjmp m1
        .eseg
data:   .db 15, 26, \"Hello, World\", end
data_w:
        .dw 0xff44, end, 0xda4e
        .cseg
m1:
        ldi r18, data_w
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_1(post_parse_result.unwrap());

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![
                    Segment {
                        items: vec![
                            (
                                CodePoint {
                                    line_num: 2,
                                    num: 2
                                },
                                Item::Instruction(
                                    Operation::Ldi,
                                    vec![
                                        InstructionOps::R8(Reg8::R16),
                                        InstructionOps::E(Expr::Ident("data".to_string())),
                                    ],
                                )
                            ),
                            (
                                CodePoint {
                                    line_num: 3,
                                    num: 2
                                },
                                Item::Instruction(
                                    Operation::Rjmp,
                                    vec![InstructionOps::E(Expr::Ident("m1".to_string()))],
                                )
                            ),
                        ],
                        t: SegmentType::Code,
                        address: 0
                    },
                    Segment {
                        items: vec![
                            (
                                CodePoint {
                                    line_num: 5,
                                    num: 2
                                },
                                Item::Directive(
                                    Directive::Db,
                                    DirectiveOps::OpList(vec![
                                        Operand::E(Expr::Const(15)),
                                        Operand::E(Expr::Const(26)),
                                        Operand::S("Hello, World".to_string()),
                                        Operand::E(Expr::Ident("end".to_string())),
                                    ]),
                                )
                            ),
                            (
                                CodePoint {
                                    line_num: 7,
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
                            ),
                        ],
                        t: SegmentType::Eeprom,
                        address: 0,
                    },
                    Segment {
                        items: vec![(
                            CodePoint {
                                line_num: 10,
                                num: 2
                            },
                            Item::Instruction(
                                Operation::Ldi,
                                vec![
                                    InstructionOps::R8(Reg8::R18),
                                    InstructionOps::E(Expr::Ident("data_w".to_string()))
                                ]
                            )
                        ),],
                        t: SegmentType::Code,
                        address: 2,
                    }
                ],
                equs: HashMap::new(),
                labels: hashmap! {
                    "data".to_string() => (SegmentType::Eeprom, 0),
                    "data_w".to_string() => (SegmentType::Eeprom, 0xf),
                    "m1".to_string() => (SegmentType::Code, 2),
                },
                device: Device::new(0)
            }
        );

        let parse_result = parse_str(".dseg\ndata: .byte 1\ncounter: .byte 2");

        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_1(post_parse_result.unwrap());

        assert_eq!(
            build_result.unwrap(),
            BuildResultPass1 {
                segments: vec![Segment {
                    items: vec![],
                    t: SegmentType::Data,
                    address: 0x60,
                },],
                equs: HashMap::new(),
                labels: hashmap! {
                    "data".to_string() => (SegmentType::Data, 0x60),
                    "counter".to_string() => (SegmentType::Data, 0x61),
                },
                device: Device::new(0)
            }
        );
    }
}
