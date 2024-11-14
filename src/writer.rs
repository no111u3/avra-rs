//! Contains hex writer of AVRA-rs

use crate::builder::BuildResult;
use failure::Error;
use ihex::Record;
use std::{fs::File, io::Write, path::PathBuf};

pub struct GenerateResult {
    code: String,
    eeprom: String,
}

fn generate_hex_from_segment(segment: &[u8]) -> Result<String, Error> {
    let mut records = vec![];
    if segment.len() > 0 {
        records.push(Record::ExtendedSegmentAddress(0x0));

        for (i, chunk) in segment.chunks(16).enumerate() {
            records.push(Record::Data {
                offset: i as u16 * 16,
                value: chunk.to_vec(),
            });
        }
    }
    records.push(Record::EndOfFile);

    let hex = ihex::create_object_file_representation(&records)?;

    Ok(hex)
}

fn generate_hex(br: &BuildResult) -> Result<GenerateResult, Error> {
    let code = generate_hex_from_segment(&br.code)?;

    let eeprom = generate_hex_from_segment(&br.eeprom)?;

    Ok(GenerateResult { code, eeprom })
}

pub fn write_code_hex(path: PathBuf, br: &BuildResult) -> Result<(), Error> {
    let output = generate_hex(br)?;
    let mut file_output = File::create(path)?;
    file_output.write_all(output.code.replace("\n", "\r\n").as_bytes())?;
    file_output.write(b"\r\n")?;

    Ok(())
}

pub fn write_eeprom_hex(path: PathBuf, br: &BuildResult) -> Result<(), Error> {
    let output = generate_hex(br)?;
    let mut file_output = File::create(path)?;
    file_output.write_all(output.eeprom.replace("\n", "\r\n").as_bytes())?;
    file_output.write(b"\r\n")?;

    Ok(())
}

#[cfg(test)]
mod writer_tests {
    use super::*;

    #[test]
    fn check_empty() {
        let result = generate_hex(&BuildResult {
            code: vec![],
            eeprom: vec![],
            flash_size: 4194304,
            eeprom_size: 65536,
            ram_size: 0,
            ram_filling: 0,
            messages: vec![],
        })
        .unwrap();

        assert_eq!(result.code, ":00000001FF\n".to_string());
    }

    #[test]
    fn check_code() {
        let build_result = BuildResult {
            code: vec![
                0xf, 0x92, 0x10, 0x2d, 0x1f, 0x5f, 0xa, 0xf4, 0xfc, 0xcf, 0x1f, 0x90, 0xea, 0xe0,
                0xf0, 0xe0, 0x5, 0x91, 0xb, 0xc0, 0xf, 0x1a, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c,
                0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x42, 0x0, 0x44, 0xff, 0x42, 0x0, 0x4e, 0xda,
                0x22, 0xe1,
            ],
            eeprom: vec![],
            flash_size: 0,
            eeprom_size: 0,
            ram_size: 0,
            ram_filling: 0,
            messages: vec![],
        };

        let result = generate_hex(&build_result).unwrap();

        assert_eq!(
            result.code,
            ":020000020000FC
:100000000F92102D1F5F0AF4FCCF1F90EAE0F0E082
:1000100005910BC00F1A48656C6C6F2C20576F72DE
:0C0020006C64420044FF42004EDA22E112
:00000001FF\n"
                .to_string()
        );
    }
}
