//! Contains zero, first and second pass builder of AVRA-rs

pub mod pass0;
pub mod pass1;
pub mod pass2;

use crate::builder::pass0::build_pass_0 as pass0;
use crate::builder::pass1::build_pass_1 as pass1;
use crate::builder::pass2::build_pass_2 as pass2;
use crate::parser::{parse_file, parse_str, Paths};

use failure::Error;
use std::path::PathBuf;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResult {
    pub code: Vec<u8>,
    pub eeprom: Vec<u8>,
}

pub fn build_str(source: &str) -> Result<BuildResult, Error> {
    let parsed = parse_str(source)?;

    let passed_0 = pass0(parsed)?;

    let passed_1 = pass1(passed_0)?;

    let passed_2 = pass2(passed_1)?;

    Ok(BuildResult {
        code: passed_2.code,
        eeprom: passed_2.eeprom,
    })
}

pub fn build_file(path: PathBuf, paths: Paths) -> Result<BuildResult, Error> {
    let parsed = parse_file(path, paths)?;

    let passed_0 = pass0(parsed)?;

    let passed_1 = pass1(passed_0)?;

    let passed_2 = pass2(passed_1)?;

    Ok(BuildResult {
        code: passed_2.code,
        eeprom: passed_2.eeprom,
    })
}

#[cfg(test)]
mod builder_tests {
    use super::*;
    use crate::utility::get_standard_includes;
    use maplit::btreeset;
    use std::path::PathBuf;

    #[test]
    fn check_empty() {
        let built = build_str("").unwrap();

        assert_eq!(
            built,
            BuildResult {
                code: vec![],
                eeprom: vec![]
            }
        );
    }

    #[test]
    fn check_main_options() {
        let asm_code = "
        .cseg
        .org 0x0
        .equ end = 0x42
        push r0
m_begin:
        mov r17, r0
        subi r17, (-1)
        brpl m0
        rjmp m_begin

m0:     pop r1
        ldi r30, low (data)
        ldi r31, high(data)
        lpm r16, Z+
        rjmp m1
data:   .db 15, 26, \"Hello, World\", end
data_w:
        .dw 0xff44, end, 0xda4e
m1:
        ldi r18, data_w
        ";

        let built = build_str(asm_code).unwrap();

        let build_success_result = BuildResult {
            code: vec![
                0xf, 0x92, 0x10, 0x2d, 0x1f, 0x5f, 0xa, 0xf4, 0xfc, 0xcf, 0x1f, 0x90, 0xea, 0xe0,
                0xf0, 0xe0, 0x5, 0x91, 0xb, 0xc0, 0xf, 0x1a, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c,
                0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x42, 0x0, 0x44, 0xff, 0x42, 0x0, 0x4e, 0xda,
                0x22, 0xe1,
            ],
            eeprom: vec![],
        };

        assert_eq!(built, build_success_result);

        let built = build_file(
            PathBuf::from("tests/builder_simple.asm"),
            btreeset! { PathBuf::from("includes") },
        )
        .unwrap();

        assert_eq!(built, build_success_result);

        let built = build_file(
            PathBuf::from("tests/builder_simple.asm"),
            btreeset! { get_standard_includes() },
        )
        .unwrap();

        assert_eq!(built, build_success_result);
    }
}
