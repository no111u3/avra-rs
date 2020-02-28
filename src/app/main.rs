//! Main application code of AVRA-rs

mod opt;

use crate::opt::Opt;

use avra_lib::{builder::build_file, writer::write_code_hex};

use maplit::btreeset;
use std::path::Path;
use structopt::StructOpt;

fn main() {
    let opt = Opt::from_args();

    let file_name = opt
        .source
        .clone()
        .into_os_string()
        .into_string()
        .unwrap_or(String::new());

    match build_file(opt.source.clone(), btreeset! {}) {
        Ok(built) => {
            // TODO: Add check and write eeprom
            // write to file
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

                match write_code_hex(outpath, built) {
                    Ok(()) => {}
                    Err(e) => println!(
                        "Failed to generate and write hex file {}, with error {}",
                        file_name, e
                    ),
                }
            } else {
                println!("Nothing to write for file {}", file_name);
            }
        }
        Err(e) => {
            println!("Failed to build file {}, with error {}", file_name, e);
        }
    }
}
