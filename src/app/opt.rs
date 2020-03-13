//! Command options for main application of AVRA-rs

use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = env!("CARGO_PKG_NAME"), about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct Opt {
    /// Source file name
    #[structopt(short, long, parse(from_os_str))]
    pub source: PathBuf,
    /// Output firmware file name, if not provided build in same name and same place as source file
    #[structopt(short, long, parse(from_os_str))]
    pub output: Option<PathBuf>,
    /// Output eeprom file name, if not provided build in same name and same place as source file
    #[structopt(short, long, parse(from_os_str))]
    pub eeprom: Option<PathBuf>,
    /// Verbosity mode
    #[structopt(short, long)]
    pub verbosity: bool,
}
