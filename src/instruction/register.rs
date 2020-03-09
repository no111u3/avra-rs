use strum_macros::{Display, EnumString};

/// Main core registers
#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Reg8 {
    // Lower part of registers
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // Higher part of registers
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
    R23,
    R24,
    R25,
    R26,
    R27,
    R28,
    R29,
    R30,
    R31,
}

impl Reg8 {
    pub fn number(self) -> u16 {
        self as u16
    }
}

/// Combined core registers
#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Reg16 {
    X,
    Y,
    Z,
}
