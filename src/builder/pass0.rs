//! Contains zero pass builder of AVRA-rs
use std::collections::HashMap;

use crate::device::Device;
use crate::expr::Expr;
use crate::parser::{parse, ParseResult, Segment};

use failure::Error;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildResultPass0 {
    // collect of segments
    pub segments: Vec<Segment>,
    // equals
    pub equs: HashMap<String, Expr>,
    // device
    pub device: Option<Device>,
}

impl BuildResultPass0 {
    pub fn new() -> Self {
        Self {
            segments: vec![],
            equs: HashMap::new(),
            device: Some(Device::new(0)),
        }
    }
}

pub fn build_pass_0(parsed: ParseResult) -> Result<BuildResultPass0, Error> {
    let mut segments = vec![];

    for segment in parsed.segments {
        segments.push(segment.clone());
    }

    Ok(BuildResultPass0 {
        segments,
        equs: parsed.equs,
        device: parsed.device,
    })
}
