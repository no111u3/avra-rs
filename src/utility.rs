use dirs::config_dir;

use std::path::PathBuf;

pub fn get_standard_includes() -> PathBuf {
    let mut standard_includes = config_dir().unwrap();

    standard_includes.push(env!("CARGO_PKG_NAME"));
    standard_includes.push(PathBuf::from("includes"));

    standard_includes
}
