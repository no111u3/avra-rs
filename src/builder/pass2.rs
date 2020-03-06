//! Contains second pass builder of AVRA-rs

use crate::builder::pass1::BuildResultPass1;
use crate::context::Context;
use crate::device::Device;
use crate::directive::{Directive, DirectiveOps, Operand};
use crate::expr::Expr;
use crate::instruction::{process, register::Reg8};
use crate::parser::{Item, Segment, SegmentType};

use std::collections::HashMap;
use std::str::FromStr;

use failure::{bail, Error};

use maplit::hashmap;
use std::cell::RefCell;

#[derive(Clone, PartialEq, Eq, Debug)]
struct Pass2Context {
    // equals
    equs: HashMap<String, Expr>,
    // labels
    labels: HashMap<String, (SegmentType, u32)>,
    // defs
    defs: RefCell<HashMap<String, Reg8>>,
    // sets
    sets: RefCell<HashMap<String, Expr>>,
    // device
    device: Device,
}

impl Context for Pass2Context {
    fn get_equ(&self, name: &String) -> Option<Expr> {
        self.equs.get(name).map(|x| x.clone())
    }

    fn get_label(&self, name: &String) -> Option<(SegmentType, u32)> {
        self.labels.get(name).map(|x| x.clone())
    }

    fn get_def(&self, name: &String) -> Option<Reg8> {
        self.defs.borrow().get(name).map(|x| x.clone())
    }

    fn get_set(&self, name: &String) -> Option<Expr> {
        self.sets.borrow().get(name).map(|x| x.clone())
    }

    fn set_def(&self, name: String, value: Reg8) -> Option<Reg8> {
        if self.exist(&name) {
            None
        } else {
            self.defs.borrow_mut().insert(name, value)
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass2 {
    pub code_start_address: u32,
    pub code: Vec<u8>,
    pub eeprom_start_address: u32,
    pub eeprom: Vec<u8>,
}

pub fn build_pass_2(pass1: BuildResultPass1) -> Result<BuildResultPass2, Error> {
    let mut code = vec![];
    let code_start_address = 0x0;
    let mut eeprom = vec![];
    let eeprom_start_address = 0x0;

    let context = Pass2Context {
        equs: pass1.equs,
        labels: pass1.labels,
        defs: RefCell::new(hashmap! {}),
        sets: RefCell::new(hashmap! {}),
        device: pass1.device,
    };

    for segment in pass1.segments {
        // TODO: Rewrite to correct ordering of segment offsets and sizes
        match segment.t {
            SegmentType::Code => {
                // pad to address
                for _ in (code_start_address as i32)..segment.address as i32 - code.len() as i32 / 2
                {
                    // Pushing nop command for spaces
                    code.extend(vec![0x00, 0x00]);
                }
            }
            SegmentType::Eeprom => {
                // pad to address
                for _ in (eeprom_start_address as i32)..segment.address as i32 - code.len() as i32 {
                    // Pushing empty data for spaces
                    code.extend(vec![0x00]);
                }
            }
            // Data not writed anywhere
            SegmentType::Data => {}
        }

        let fragment = pass_2_internal(&segment, &context)?;

        match segment.t {
            SegmentType::Code => {
                code.extend(fragment);
            }
            SegmentType::Eeprom => {
                eeprom.extend(fragment);
            }
            // Data not writed anywhere
            SegmentType::Data => {}
        }
    }

    Ok(BuildResultPass2 {
        code_start_address,
        code,
        eeprom_start_address,
        eeprom,
    })
}

fn pass_2_internal(segment: &Segment, context: &Pass2Context) -> Result<Vec<u8>, Error> {
    let mut code_fragment = vec![];

    let mut cur_address = segment.address;

    for (line, item) in segment.items.iter() {
        match item {
            Item::Instruction(op, op_args) => {
                if context.device.check_operation(op) {
                    let complete_op = match process(&op, &op_args, cur_address, context) {
                        Ok(ok) => ok,
                        Err(e) => bail!("{}, {}", e, line),
                    };
                    cur_address += complete_op.len() as u32 / 2;
                    code_fragment.extend(complete_op);
                } else {
                    bail!(
                        "instruction {} is not allowed for current device, {}",
                        op,
                        line
                    )
                }
            }
            Item::Directive(d, d_op) => match d {
                Directive::Db => {
                    if let DirectiveOps::OpList(_) = d_op {
                        let data = match d_op.get_bytes(context) {
                            Ok(ok) => ok,
                            Err(e) => bail!("{}, {}", e, line),
                        };
                        cur_address += if let SegmentType::Code = segment.t {
                            data.len() as u32 / 2
                        } else {
                            data.len() as u32
                        };
                        code_fragment.extend(data);
                    }
                }
                Directive::Dw => {
                    if let DirectiveOps::OpList(_) = d_op {
                        let data = match d_op.get_words(context) {
                            Ok(ok) => ok,
                            Err(e) => bail!("{}, {}", e, line),
                        };
                        cur_address += if let SegmentType::Code = segment.t {
                            data.len() as u32 / 2
                        } else {
                            data.len() as u32
                        };
                        code_fragment.extend(data);
                    }
                }
                Directive::Def => {
                    if let DirectiveOps::Assign(Expr::Ident(alias), Expr::Ident(register)) = d_op {
                        if let Some(_) = context.set_def(
                            alias.to_lowercase(),
                            Reg8::from_str(register.to_lowercase().as_str()).unwrap(),
                        ) {
                            // TODO: add display current string of mistake and previous location
                            bail!("Identifier {} is used twice, {}", alias, line);
                        }
                    }
                }
                Directive::Undef => {
                    if let DirectiveOps::OpList(values) = d_op {
                        if let Operand::E(Expr::Ident(name)) = &values[0] {
                            if let None = context.defs.borrow_mut().remove(name) {
                                bail!("Identifier {} isn't defined, {}", name, line);
                            }
                        }
                    }
                }
                Directive::Set => {
                    if let DirectiveOps::Assign(Expr::Ident(name), expr) = d_op {
                        let value = expr.run(context)?;
                        if context.exist(name) {
                            let mut sets = context.sets.borrow_mut();
                            if let Some(_) = sets.get(name) {
                                sets.insert(name.clone(), Expr::Const(value));
                            } else {
                                // TODO: add display current string of mistake and previous location
                                bail!("Identifier {} is used twice, {}", name, line);
                            }
                        } else {
                            context
                                .sets
                                .borrow_mut()
                                .insert(name.clone(), Expr::Const(value));
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    Ok(code_fragment)
}

#[cfg(test)]
mod builder_tests {
    use super::*;
    use crate::builder::pass0::{build_pass_0, BuildResultPass0};
    use crate::builder::pass1::build_pass_1;
    use crate::parser::{parse_str, ParseResult};

    #[test]
    fn check_empty() {
        let build_result = build_pass_2(build_pass_1(BuildResultPass0::new()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_no_args() {
        let parse_result = parse_str(
            "
        nop
        ret
        seh
        clh
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x00, 0x00, 0x08, 0x95, 0x58, 0x94, 0xd8, 0x94],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_one_arg() {
        let parse_result = parse_str(
            "
        push r0
        lsl r0
        swap r0
        pop r1
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0xf, 0x92, 0x0, 0xc, 0x2, 0x94, 0x1f, 0x90],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );

        let parse_result = parse_str(
            "
        tst r1
error:
        brpl exit
exit:
        rjmp error
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x11, 0x20, 0x2, 0xf4, 0xfe, 0xcf],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_two_args() {
        let parse_result = parse_str(
            "
        ldi r16, 1 << 2 | 1 << 1
        mov r0, r16
        subi r16, (-1)
        sts data, r16
data:
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x6, 0xe0, 0x0, 0x2e, 0xf, 0x5f, 0x0, 0x93, 0x5, 0x0],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );

        let parse_result = parse_str(
            "
        ld r17, X
        ld r18, Y+
        ld r19, -Z
        st X+, r19
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x1c, 0x91, 0x29, 0x91, 0x32, 0x91, 0x3d, 0x93],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );

        let parse_result = parse_str(
            "
        ldd r25, Z+2
        std Z+6, r24
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x92, 0x81, 0x86, 0x83],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_db_dw() {
        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r16, data
data:   .db 15, 26, \"Hello, World\", end  
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![
                    0x1, 0xe0, 0xf, 0x1a, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x57, 0x6f,
                    0x72, 0x6c, 0x64, 0x0, 0x0
                ],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );

        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r18, data_w 
data_w:
        .dw 0xff44, end, 0xda4e
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x21, 0xe0, 0x44, 0xff, 0x0, 0x0, 0x4e, 0xda],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_cseg_org() {
        let parse_result = parse_str(
            "
        nop
        .org 0x2
        seh
        .cseg
        .org 0x5
        clh
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x00, 0x00, 0x00, 0x00, 0x58, 0x94, 0x00, 0x00, 0x00, 0x00, 0xd8, 0x94],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }

    #[test]
    fn check_eseg() {
        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r18, data_w 
        .eseg
data_w:
        .dw 0xff44, end, 0xda4e
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x20, 0xe0],
                eeprom_start_address: 0x0,
                eeprom: vec![0x44, 0xff, 0x0, 0x0, 0x4e, 0xda],
            }
        );
    }

    #[test]
    fn check_dseg() {
        let parse_result = parse_str(
            "
.dseg
data: .byte 1
counter:
    .byte 2
        .cseg
        lds r18, data
        lds r16, counter
        lds r17, counter+1
        ",
        );
        let post_parse_result = build_pass_0(parse_result.unwrap());

        let build_result = build_pass_2(build_pass_1(post_parse_result.unwrap()).unwrap());
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x20, 0x91, 0x60, 0x0, 0x0, 0x91, 0x61, 0x0, 0x10, 0x91, 0x62, 0x0],
                eeprom_start_address: 0x0,
                eeprom: vec![],
            }
        );
    }
}
