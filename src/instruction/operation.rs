use strum_macros::{Display, EnumString};

/// Status register flags
#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum SFlags {
    /// Carry
    C,
    /// Zero    
    Z,
    /// Negative
    N,
    /// Twos complement overflow
    V,
    /// Test
    S,
    /// Half carry
    H,
    /// Transfer bit used
    T,
    /// Interrupt
    I,
}

impl SFlags {
    pub fn number(self) -> u16 {
        self as u16
    }
}

/// Conditinal branch type
#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum BranchT {
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Carry set
    Cs,
    /// Carry clear
    Cc,
    /// Same or higher
    Sh,
    /// Lower
    Lo,
    /// Minus
    Mi,
    /// Plus
    Pl,
    /// Greater or equal, signed
    Ge,
    /// Less than, signed
    Lt,
    /// Half carry set
    Hs,
    /// Half carry cleared
    Hc,
    /// Transfer set
    Ts,
    /// Transfer cleared
    Tc,
    /// Overflow set
    Vs,
    /// Overflow cleared
    Vc,
    /// Interrupt enabled
    Ie,
    /// Interrupt disabled
    Id,
    /// Status flag set
    Bs,
    /// Status flag cleared
    Bc,
}

impl BranchT {
    pub fn number(self) -> u16 {
        match self {
            BranchT::Eq => 0x1,
            BranchT::Ne => 0x401,
            BranchT::Cs | BranchT::Lo => 0x0,
            BranchT::Cc | BranchT::Sh => 0x400,
            BranchT::Mi => 0x2,
            BranchT::Pl => 0x402,
            BranchT::Ge => 0x404,
            BranchT::Lt => 0x4,
            BranchT::Hs => 0x5,
            BranchT::Hc => 0x405,
            BranchT::Ts => 0x6,
            BranchT::Tc => 0x406,
            BranchT::Vs => 0x3,
            BranchT::Vc => 0x403,
            BranchT::Ie => 0x7,
            BranchT::Id => 0x407,
            BranchT::Bc => 0x400,
            BranchT::Bs => 0x0,
        }
    }
}

/// Operations
#[derive(Clone, PartialEq, Eq, Debug, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Operation {
    /// Arithmetic and logic instructions
    Add,
    Adc,
    /// Add Immediate to Word
    Adiw,
    Sub,
    Subi,
    Sbc,
    Sbci,
    /// Substract Immediate from Word
    Sbiw,
    And,
    Andi,
    Or,
    Ori,
    Eor,
    Com,
    Neg,
    /// Set bit(s) in register
    Sbr,
    /// Clear bit(s) in register
    Cbr,
    Inc,
    Dec,
    Tst,
    /// Clear all bits in register
    Clr,
    /// Set all bits in register
    Ser,
    Mul,
    /// Multiply signed
    Muls,
    /// Multiply signed with unsigned
    Mulsu,
    /// Fractional multiply
    Fmul,
    /// Fractional multiply signed
    Fmuls,
    /// Fractional multiply signed with unsigned
    Fmulsu,

    /// Branch instructions

    /// Relative jump
    Rjmp,
    /// Indirect jump
    Ijmp,
    /// Extended indirect jump
    Eijmp,
    Jmp,
    /// Relative call
    Rcall,
    /// Indirect call
    Icall,
    /// Extended inderect call
    Eicall,
    Call,
    /// Subroutine return
    Ret,
    /// Interrupt return
    Reti,
    /// Compare, skip if equal
    Cpse,
    /// Compare
    Cp,
    /// Compare with carry
    Cpc,
    /// Compare with immediate
    Cpi,
    /// Conditinal branch
    #[strum(disabled = "true")]
    Br(BranchT),
    /// Skip if bit in I/O register is cleared
    Sbic,
    /// Skip if bit in I/O register is set
    Sbis,
    /// Skip if bit in register is cleared
    Sbrc,
    /// Skip if bit in register is set
    Sbrs,

    /// Data transfer instructions
    Mov,
    /// Copy register pair
    Movw,
    /// Load immediate
    Ldi,
    /// Load direct from data space
    Lds,

    /// Load inderect and none,post-increment or pre-decrement
    Ld,
    /// Load inderect with displacement
    Ldd,
    /// Store direct to data space
    Sts,
    /// Store inderect and none,post-increment or pre-decrement
    St,
    /// Store inderect with displacement
    Std,
    /// Load program memory to R0, Rx and Rx and post-increment
    Lpm,
    /// Extended load program memory to R0, Rx and Rx and post-increment
    Elpm,
    /// Store program memory
    Spm,
    /// In from I/O location
    In,
    /// Out to I/O location
    Out,
    /// Clear bit in I/O location
    Cbi,
    /// Set bit in I/O location
    Sbi,
    Push,
    Pop,

    /// Bit and Bit-test instructions

    /// Logical shift left
    Lsl,
    /// Logical shift right
    Lsr,
    /// Rotate left through carry
    Rol,
    /// Rotate right through carry
    Ror,
    /// Arithmetic shift right
    Asr,
    /// Swap nibbles
    Swap,
    /// Flag set
    Bset,
    /// Flag clear
    Bclr,
    /// Bit store from bit in register to T flag in SREG
    Bst,
    /// Bit load from the T flag in SREG to a bit in register
    Bld,
    /// Set flag in status register
    #[strum(disabled = "true")]
    Se(SFlags),
    /// Clear flag in status register
    #[strum(disabled = "true")]
    Cl(SFlags),

    /// Mcu control instructions

    /// Break to debugger
    Break,
    Nop,
    /// Supend cpu core and go to sleel mcu
    Sleep,
    /// Watchdog reset
    Wdr,

    /// For extend and macross support
    #[strum(disabled = "true")]
    Custom(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Info {
    pub len: u32,
    pub op_code: u16,
}

impl Operation {
    /// Returns how many words the instruction takes up
    pub fn info(&self) -> Info {
        match self {
            // Rd, Rr   0000 11rd dddd rrrr
            Operation::Add => Info {
                len: 1,
                op_code: 0x0c00,
            },
            // Rd, Rr   0001 11rd dddd rrrr
            Operation::Adc => Info {
                len: 1,
                op_code: 0x1c00,
            },
            // Rd, K    1001 0110 KKdd KKKK
            Operation::Adiw => Info {
                len: 1,
                op_code: 0x9600,
            },
            // Rd, Rr   0001 10rd dddd rrrr
            Operation::Sub => Info {
                len: 1,
                op_code: 0x1800,
            },
            // Rd, K    0101 KKKK dddd KKKK
            Operation::Subi => Info {
                len: 1,
                op_code: 0x5000,
            },
            // Rd, Rr   0000 10rd dddd rrrr
            Operation::Sbc => Info {
                len: 1,
                op_code: 0x0800,
            },
            // Rd, K    0100 KKKK dddd KKKK
            Operation::Sbci => Info {
                len: 1,
                op_code: 0x4000,
            },
            // Rd, K    1001 0111 KKdd KKKK
            Operation::Sbiw => Info {
                len: 1,
                op_code: 0x9700,
            },
            // Rd, Rr   0010 00rd dddd rrrr
            Operation::And => Info {
                len: 1,
                op_code: 0x2000,
            },
            // Rd, K    0111 KKKK dddd KKKK
            Operation::Andi => Info {
                len: 1,
                op_code: 0x7000,
            },
            // Rd, Rr   0010 10rd dddd rrrr
            Operation::Or => Info {
                len: 1,
                op_code: 0x2800,
            },
            // Rd, K    0110 KKKK dddd KKKK
            Operation::Ori => Info {
                len: 1,
                op_code: 0x6000,
            },
            // Rd, Rr   0010 01rd dddd rrrr
            Operation::Eor => Info {
                len: 1,
                op_code: 0x2400,
            },
            // Rd       1001 010d dddd 0000
            Operation::Com => Info {
                len: 1,
                op_code: 0x9400,
            },
            // Rd       1001 010d dddd 0001
            Operation::Neg => Info {
                len: 1,
                op_code: 0x9401,
            },
            // Rd, K    0110 KKKK dddd KKKK
            Operation::Sbr => Info {
                len: 1,
                op_code: 0x6000,
            },
            // Rd, K    0111 KKKK dddd KKKK ~K
            Operation::Cbr => Info {
                len: 1,
                op_code: 0x7000,
            },
            // Rd       1001 010d dddd 0011
            Operation::Inc => Info {
                len: 1,
                op_code: 0x9403,
            },
            // Rd       1001 010d dddd 1010
            Operation::Dec => Info {
                len: 1,
                op_code: 0x940a,
            },
            // Rd       0010 00dd dddd dddd
            Operation::Tst => Info {
                len: 1,
                op_code: 0x2000,
            },
            // Rd       0010 01dd dddd dddd
            Operation::Clr => Info {
                len: 1,
                op_code: 0x2400,
            },
            // Rd       1110 1111 dddd 1111
            Operation::Ser => Info {
                len: 1,
                op_code: 0xef0f,
            },
            // Rd, Rr   1001 11rd dddd rrrr
            Operation::Mul => Info {
                len: 1,
                op_code: 0x9c00,
            },
            // Rd, Rr   0000 0010 dddd rrrr
            Operation::Muls => Info {
                len: 1,
                op_code: 0x0200,
            },
            // Rd, Rr   0000 0011 0ddd 0rrr
            Operation::Mulsu => Info {
                len: 1,
                op_code: 0x0300,
            },
            // Rd, Rr   0000 0011 0ddd 1rrr
            Operation::Fmul => Info {
                len: 1,
                op_code: 0x0308,
            },
            // Rd, Rr   0000 0011 1ddd 0rrr
            Operation::Fmuls => Info {
                len: 1,
                op_code: 0x0380,
            },
            // Rd, Rr   0000 0011 1ddd 1rrr
            Operation::Fmulsu => Info {
                len: 1,
                op_code: 0x0388,
            },
            // k        1100 kkkk kkkk kkkk
            Operation::Rjmp => Info {
                len: 1,
                op_code: 0xc000,
            },
            //          1001 0100 0000 1001
            Operation::Ijmp => Info {
                len: 1,
                op_code: 0x9409,
            },
            //          1001 0100 0001 1001
            Operation::Eijmp => Info {
                len: 1,
                op_code: 0x9419,
            },
            // k        1001 010k kkkk 110k + 16k
            Operation::Jmp => Info {
                len: 2,
                op_code: 0x940c,
            },
            // k        1101 kkkk kkkk kkkk
            Operation::Rcall => Info {
                len: 1,
                op_code: 0xd000,
            },
            //          1001 0101 0000 1001
            Operation::Icall => Info {
                len: 1,
                op_code: 0x9509,
            },
            //          1001 0101 0001 1001
            Operation::Eicall => Info {
                len: 1,
                op_code: 0x9519,
            },
            // k        1001 010k kkkk 111k + 16k
            Operation::Call => Info {
                len: 2,
                op_code: 0x940e,
            },
            //          1001 0101 0000 1000
            Operation::Ret => Info {
                len: 1,
                op_code: 0x9508,
            },
            //          1001 0101 0001 1000
            Operation::Reti => Info {
                len: 1,
                op_code: 0x9518,
            },
            // Rd, Rr   0001 00rd dddd rrrr
            Operation::Cpse => Info {
                len: 1,
                op_code: 0x1000,
            },
            // Rd, Rr   0001 01rd dddd rrrr
            Operation::Cp => Info {
                len: 1,
                op_code: 0x1400,
            },
            // Rd, Rr   0000 01rd dddd rrrr
            Operation::Cpc => Info {
                len: 1,
                op_code: 0x0400,
            },
            // Rd, K    0011 KKKK dddd KKKK
            Operation::Cpi => Info {
                len: 1,
                op_code: 0x3000,
            },
            // s, k     1111 0skk kkkk ksss
            Operation::Br(_) => Info {
                len: 1,
                op_code: 0xf000,
            },
            // P, b     1001 1001 PPPP Pbbb
            Operation::Sbic => Info {
                len: 1,
                op_code: 0x9900,
            },
            // P, b     1001 1011 PPPP Pbbb
            Operation::Sbis => Info {
                len: 1,
                op_code: 0x9b00,
            },
            // Rr, b    1111 110r rrrr 0bbb
            Operation::Sbrc => Info {
                len: 1,
                op_code: 0xfc00,
            },
            // Rr, b    1111 111r rrrr 0bbb
            Operation::Sbrs => Info {
                len: 1,
                op_code: 0xfe00,
            },
            // Rd, Rr   0010 11rd dddd rrrr
            Operation::Mov => Info {
                len: 1,
                op_code: 0x2c00,
            },
            // Rd, Rr   0000 0001 dddd rrrr
            Operation::Movw => Info {
                len: 1,
                op_code: 0x0100,
            },
            // Rd, K    1110 KKKK dddd KKKK
            Operation::Ldi => Info {
                len: 1,
                op_code: 0xe000,
            },
            // Rd, k    1001 000d dddd 0000 + 16k
            Operation::Lds => Info {
                len: 2,
                op_code: 0x9000,
            },
            // Rd, -I+  100i 000d dddd ii-+
            Operation::Ld => Info {
                len: 1,
                op_code: 0x8000,
            },
            // Rd, Y+q  10q0 qq0d dddd iqqq
            Operation::Ldd => Info {
                len: 1,
                op_code: 0x8000,
            },
            // k, Rr    1001 001d dddd 0000 + 16k
            Operation::Sts => Info {
                len: 2,
                op_code: 0x9200,
            },
            // -I+, Rr  100i 001d dddd ii-+
            Operation::St => Info {
                len: 1,
                op_code: 0x8200,
            },
            // Y+q, Rr  10q0 qq1r rrrr iqqq
            Operation::Std => Info {
                len: 1,
                op_code: 0x8200,
            },
            //          1001 0101 1100 1000
            // Rd, Z    1001 000d dddd 0100
            // Rd, Z+   1001 000d dddd 0101
            Operation::Lpm => Info {
                len: 1,
                op_code: 0x9000,
            },
            //          1001 0101 1101 1000
            // Rd, Z    1001 000d dddd 0110
            // Rd, Z+   1001 000d dddd 0111
            Operation::Elpm => Info {
                len: 1,
                op_code: 0x9000,
            },
            //          1001 0101 1110 1000
            Operation::Spm => Info {
                len: 1,
                op_code: 0x95e8,
            },
            // Rd, P    1011 0PPd dddd PPPP
            Operation::In => Info {
                len: 1,
                op_code: 0xb000,
            },
            // P, Rr    1011 1PPr rrrr PPPP
            Operation::Out => Info {
                len: 1,
                op_code: 0xb800,
            },
            // P, b     1001 1000 PPPP Pbbb
            Operation::Cbi => Info {
                len: 1,
                op_code: 0x9800,
            },
            // P, b     1001 1010 PPPP Pbbb
            Operation::Sbi => Info {
                len: 1,
                op_code: 0x9a00,
            },
            // Rr       1001 001r rrrr 1111
            Operation::Push => Info {
                len: 1,
                op_code: 0x920f,
            },
            // Rd       1001 000d dddd 1111
            Operation::Pop => Info {
                len: 1,
                op_code: 0x900f,
            },
            // Rd       0000 11dd dddd dddd
            Operation::Lsl => Info {
                len: 1,
                op_code: 0x0c00,
            },
            // Rd       1001 010d dddd 0110
            Operation::Lsr => Info {
                len: 1,
                op_code: 0x9406,
            },
            // Rd       0001 11dd dddd dddd
            Operation::Rol => Info {
                len: 1,
                op_code: 0x1c00,
            },
            // Rd       1001 010d dddd 0111
            Operation::Ror => Info {
                len: 1,
                op_code: 0x9407,
            },
            // Rd       1001 010d dddd 0101
            Operation::Asr => Info {
                len: 1,
                op_code: 0x9405,
            },
            // Rd       1001 010d dddd 0010
            Operation::Swap => Info {
                len: 1,
                op_code: 0x9402,
            },
            // s        1001 0100 0sss 1000
            Operation::Bset => Info {
                len: 1,
                op_code: 0x9408,
            },
            // s        1001 0100 1sss 1000
            Operation::Bclr => Info {
                len: 1,
                op_code: 0xfa00,
            },
            // Rr, b    1111 101r rrrr 0bbb
            Operation::Bst => Info {
                len: 1,
                op_code: 0xf800,
            },
            // Rd, b    1111 100d dddd 0bbb
            Operation::Bld => Info {
                len: 1,
                op_code: 0x9488,
            },
            //          1001 0100 0sss 1000
            Operation::Se(_) => Info {
                len: 1,
                op_code: 0x9408,
            },
            //          1001 0100 1sss 1000
            Operation::Cl(_) => Info {
                len: 1,
                op_code: 0x9488,
            },
            //          1001 0101 1001 1000
            Operation::Break => Info {
                len: 1,
                op_code: 0x9598,
            },
            //          0000 0000 0000 0000
            Operation::Nop => Info {
                len: 1,
                op_code: 0x0,
            },
            //          1001 0101 1000 1000
            Operation::Sleep => Info {
                len: 1,
                op_code: 0x9588,
            },
            //          1001 0101 1010 1000
            Operation::Wdr => Info {
                len: 1,
                op_code: 0x95a8,
            },
            // TODO: rewrite to generate custom error for these code
            Operation::Custom(_) => Info {
                len: 0,
                op_code: 0x0,
            },
        }
    }
}
