//! Contains second pass builder of AVRA-rs

use crate::builder::pass1::BuildResultPass1;
use crate::context::{CommonContext, Context};
use crate::device::Device;
use crate::directive::GetData;
use crate::expr::Expr;
use crate::instruction::{process, register::Reg8};
use crate::parser::{DataDefine, Item, Segment, SegmentType};

use std::collections::HashMap;
use std::str::FromStr;

use failure::{bail, Error};

use maplit::hashmap;
use std::cell::RefCell;

#[derive(Clone, PartialEq, Eq, Debug)]
struct Pass2Context {
    // common context
    common_context: CommonContext,
    // special
    special: RefCell<HashMap<String, Expr>>,
    // device
    device: Device,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass2 {
    pub code_start_address: u32,
    pub code: Vec<u8>,
    pub eeprom_start_address: u32,
    pub eeprom: Vec<u8>,
    pub device: Device,
    pub ram_filling: u32,
    pub messages: Vec<String>,
}

pub fn build_pass_2(
    pass1: BuildResultPass1,
    common_context: &CommonContext,
) -> Result<BuildResultPass2, Error> {
    let mut code = vec![];
    let code_start_address = 0x0;
    let mut eeprom = vec![];
    let eeprom_start_address = 0x0;

    let context = Pass2Context {
        common_context: common_context.clone(),
        special: RefCell::new(hashmap! {}),
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
                for _ in (eeprom_start_address as i32)..segment.address as i32 - eeprom.len() as i32
                {
                    // Pushing empty data for spaces
                    eeprom.extend(vec![0x00]);
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
        device: context.device,
        ram_filling: pass1.ram_filling,
        messages: pass1.messages,
    })
}

fn pass_2_internal(segment: &Segment, context: &Pass2Context) -> Result<Vec<u8>, Error> {
    let mut code_fragment = vec![];

    let mut cur_address = segment.address;

    for (line, item) in segment.items.iter() {
        context
            .common_context
            .set_special("pc".to_string(), Expr::Const(cur_address as i64));
        match item {
            Item::Instruction(op, op_args) => {
                if context.device.check_operation(op) {
                    let complete_op =
                        match process(&op, &op_args, cur_address, &context.common_context) {
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
            Item::Data(item_type, items) => {
                let data = match item_type {
                    DataDefine::Db => items.get_bytes(&context.common_context),
                    DataDefine::Dw => items.get_words(&context.common_context),
                    DataDefine::Dd => items.get_double_words(&context.common_context),
                    DataDefine::Dq => items.get_quad_words(&context.common_context),
                }?;
                cur_address += if let SegmentType::Code = segment.t {
                    data.len() as u32 / 2
                } else {
                    data.len() as u32
                };
                code_fragment.extend(data);
            }
            Item::ReserveData(size) => {
                cur_address += *size as u32;
                for _ in 0..*size {
                    code_fragment.push(0x0);
                }
            }
            Item::Def(alias, Expr::Ident(register)) => {
                if let Some(_) = context.common_context.set_def(
                    alias.to_lowercase(),
                    Reg8::from_str(register.to_lowercase().as_str()).unwrap(),
                ) {
                    // TODO: add display current string of mistake and previous location
                    bail!("Identifier {} is used twice, {}", alias, line);
                }
            }
            Item::Undef(alias) => {
                if let None = context.common_context.defs.borrow_mut().remove(alias) {
                    bail!("Identifier {} isn't defined, {}", alias, line);
                }
            }
            Item::Set(name, expr) => {
                let value = expr.run(&context.common_context)?;
                if context.common_context.exist(name) {
                    let mut sets = context.common_context.sets.borrow_mut();
                    if let Some(_) = sets.get(name) {
                        sets.insert(name.clone(), Expr::Const(value));
                    } else {
                        // TODO: add display current string of mistake and previous location
                        bail!("Identifier {} is used twice, {}", name, line);
                    }
                } else {
                    context
                        .common_context
                        .sets
                        .borrow_mut()
                        .insert(name.clone(), Expr::Const(value));
                }
            }
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
    use crate::parser::parse_str;

    #[test]
    fn check_empty() {
        let common_context = CommonContext::new();
        let build_result = build_pass_2(
            build_pass_1(BuildResultPass0::new(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_no_args() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        nop
        ret
        seh
        clh
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x00, 0x00, 0x08, 0x95, 0x58, 0x94, 0xd8, 0x94],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_one_arg() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        push r0
        lsl r0
        swap r0
        pop r1
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0xf, 0x92, 0x0, 0xc, 0x2, 0x94, 0x1f, 0x90],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        tst r1
error:
        brpl exit
exit:
        rjmp error
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x11, 0x20, 0x2, 0xf4, 0xfe, 0xcf],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_two_args() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        ldi r16, 1 << 2 | 1 << 1
        mov r0, r16
        subi r16, (-1)
        sts data, r16
data:
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x6, 0xe0, 0x0, 0x2e, 0xf, 0x5f, 0x0, 0x93, 0x5, 0x0],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        ld r17, X
        ld r18, Y+
        ld r19, -Z
        st X+, r19
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x1c, 0x91, 0x29, 0x91, 0x32, 0x91, 0x3d, 0x93],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        ldd r25, Z+2
        std Z+6, r24
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x92, 0x81, 0x86, 0x83],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_db_dw_dd_dq() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r16, data
data:   .db 15, 26, \"Hello, World\", end  
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
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
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r18, data_w 
data_w:
        .dw 0xff44, end, 0xda4e
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x21, 0xe0, 0x44, 0xff, 0x0, 0x0, 0x4e, 0xda],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        ldi r18, data_d
data_d:
        .dd 0x12345678, 0x9abcdef0
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x21, 0xe0, 0x78, 0x56, 0x34, 0x12, 0xf0, 0xde, 0xbc, 0x9a],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );

        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        ldi r18, data_q
data_q:
        .dq 0x1, 0x1000000000011000
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![
                    0x21, 0xe0, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x1, 0x0, 0x0,
                    0x0, 0x0, 0x10
                ],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_cseg_org() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        nop
        .org 0x2
        seh
        .cseg
        .org 0x5
        clh
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x00, 0x00, 0x00, 0x00, 0x58, 0x94, 0x00, 0x00, 0x00, 0x00, 0xd8, 0x94],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_eseg() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        .equ end = 0
        ldi r18, data_w 
        .eseg
data_w:
        .dw 0xff44, end, 0xda4e
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x20, 0xe0],
                eeprom_start_address: 0x0,
                eeprom: vec![0x44, 0xff, 0x0, 0x0, 0x4e, 0xda],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }

    #[test]
    fn check_dseg() {
        let common_context = CommonContext::new();
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
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x20, 0x91, 0x60, 0x0, 0x0, 0x91, 0x61, 0x0, 0x10, 0x91, 0x62, 0x0],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 3,
                messages: vec![],
            }
        );
    }

    #[test]
    fn relatives_branch() {
        let common_context = CommonContext::new();
        let parse_result = parse_str(
            "
        subi r16, 1
        breq pc-1
        rjmp pc
        ",
            &common_context,
        );
        let post_parse_result = build_pass_0(parse_result.unwrap(), &common_context);

        let build_result = build_pass_2(
            build_pass_1(post_parse_result.unwrap(), &common_context).unwrap(),
            &common_context,
        );
        assert_eq!(
            build_result.unwrap(),
            BuildResultPass2 {
                code_start_address: 0x0,
                code: vec![0x1, 0x50, 0xf1, 0xf3, 0xff, 0xcf],
                eeprom_start_address: 0x0,
                eeprom: vec![],
                device: Device::new(0),
                ram_filling: 0,
                messages: vec![],
            }
        );
    }
}
