//! Main application code of AVRA-rs

mod opt;

use crate::opt::Opt;

use avra_lib::{
    builder::build_file,
    utility::get_standard_includes,
    writer::{write_code_hex, write_eeprom_hex},
};

use std::path::Path;

use maplit::btreeset;
use structopt::StructOpt;

fn main() {
    let opt = Opt::from_args();

    let file_name = opt
        .source
        .clone()
        .into_os_string()
        .into_string()
        .unwrap_or(String::new());

    match build_file(opt.source.clone(), btreeset! { get_standard_includes() }) {
        Ok(built) => {
            // write to file code
            if !built.code.is_empty() {
                let outpath = if let Some(output) = opt.output {
                    output
                } else {
                    let mut source_parent = opt
                        .source
                        .clone()
                        .parent()
                        .unwrap_or(&Path::new("."))
                        .to_path_buf();
                    let mut out_file_name = String::from(
                        opt.source
                            .as_path()
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap_or(""),
                    );
                    out_file_name += ".hex";

                    source_parent.push(out_file_name);

                    source_parent
                };

                match write_code_hex(outpath, &built) {
                    Ok(()) => {}
                    Err(e) => println!(
                        "Failed to generate and write hex file {}, with error {}",
                        file_name, e
                    ),
                }
            } else {
                println!("Nothing to write of code for file {}", file_name);
            }
            // write to file eeprom
            if !built.eeprom.is_empty() {
                let outpath = if let Some(output) = opt.eeprom {
                    output
                } else {
                    let mut source_parent = opt
                        .source
                        .clone()
                        .parent()
                        .unwrap_or(&Path::new("."))
                        .to_path_buf();
                    let mut out_file_name = String::from(
                        opt.source
                            .as_path()
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap_or(""),
                    );
                    out_file_name += ".eep.hex";

                    source_parent.push(out_file_name);

                    source_parent
                };

                match write_eeprom_hex(outpath, &built) {
                    Ok(()) => {}
                    Err(e) => println!(
                        "Failed to generate and write hex file {}, with error {}",
                        file_name, e
                    ),
                }
            } else {
                println!("Nothing to write of eeprom for file {}", file_name);
            }

            if opt.verbosity {
                println!(
                    "Flash: {}({}) words(bytes) of {}({}), {:.2}%",
                    built.code.len(),
                    built.code.len() * 2,
                    built.flash_size,
                    built.flash_size * 2,
                    built.code.len() as f32 / built.flash_size as f32 * 100.
                );
                println!(
                    "EEPROM: {} bytes of {}, {:.2}%",
                    built.eeprom.len(),
                    built.eeprom_size,
                    built.eeprom.len() as f32 / built.eeprom_size as f32 * 100.
                );
                println!(
                    "RAM: {} bytes of {}, {:.2}%",
                    built.ram_filling,
                    built.ram_size,
                    built.ram_filling as f32 / built.ram_size as f32 * 100.
                );
            }
        }
        Err(e) => {
            println!("Failed to build file {}, with error {}", file_name, e);
        }
    }
}
