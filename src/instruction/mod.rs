pub mod operation;
pub mod register;

use crate::expr::{Expr, GetIdent};

use crate::instruction::{
    operation::{BranchT, Operation},
    register::{Reg16, Reg8},
};

use byteorder::{ByteOrder, LittleEndian};
use failure::{bail, Error};

/// Index register operation type
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IndexOps {
    /// None - {Index}
    None(Reg16),
    /// Post-Increment - {Index}+
    PostIncrement(Reg16),
    /// Post-Increment with expr - {Index}+(expr)
    PostIncrementE(Reg16, Expr),
    /// Pre-Decrement - -{Index}
    PreDecrement(Reg16),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InstructionOps {
    R8(Reg8),
    Index(IndexOps),
    E(Expr),
}

impl InstructionOps {
    fn get_r8(&self) -> Result<Reg8, Error> {
        match self {
            Self::R8(reg8) => Ok(*reg8),
            _ => {
                bail!("Argument must be R0-R16");
            }
        }
    }

    fn get_expr(&self) -> Result<Expr, Error> {
        match self {
            Self::E(expr) => Ok(expr.clone()),
            _ => {
                bail!("Argument must be valid expression");
            }
        }
    }

    fn get_index(&self) -> Result<IndexOps, Error> {
        match self {
            Self::Index(index) => Ok(index.clone()),
            _ => {
                bail!("Argument must be valid expression");
            }
        }
    }
}

/// Process instruction - parse arguments and construct
/// correct byte representation
pub fn process(
    op: &Operation,
    op_args: &Vec<InstructionOps>,
    current_address: u32,
    constants: &dyn GetIdent,
) -> Result<Vec<u8>, Error> {
    let mut finalized_opcode = vec![];

    let mut opcode = op.info().op_code;
    let mut opcode_2part = 0u16;
    let mut long_opcode = false;

    match op {
        Operation::Add
        | Operation::Adc
        | Operation::Sub
        | Operation::Sbc
        | Operation::And
        | Operation::Or
        | Operation::Eor
        | Operation::Cpse
        | Operation::Cp
        | Operation::Cpc
        | Operation::Mov
        | Operation::Mul => {
            let d = op_args[0].get_r8()?;
            opcode |= d.number() << 4;

            let r = op_args[1].get_r8()?;
            opcode |= (r.number() & 0x10) << 5 | r.number() & 0x0f;
        }
        Operation::Adiw | Operation::Sbiw => {
            let d = op_args[0].get_r8()?;
            let d = d.number();
            if !(d == 24 || d == 26 || d == 28 || d == 30) {
                bail!("{:?} can only use registers R24, R26, R28 or R30", op);
            }
            opcode |= ((d - 24) / 2) << 4;

            let k = op_args[1].get_expr()?;
            let k = k.get_byte(constants)? as i8;
            if k < 0 || k > 63 {
                bail!("Constant out of range (0 <= k <= 63");
            }
            let k = k as u16;
            opcode |= (k & 0x30) << 2 | k & 0x0f;
        }
        Operation::Subi
        | Operation::Sbci
        | Operation::Andi
        | Operation::Ori
        | Operation::Sbr
        | Operation::Cbr
        | Operation::Cpi
        | Operation::Ldi => {
            let d = op_args[0].get_r8()?;
            if d.number() < 16 {
                bail!("{:?} can only use a high register (r16 - r31)", op);
            }
            opcode |= (d.number() & 0x0f) << 4;

            let k = op_args[1].get_expr()?;
            let mut k: u16 = k.get_byte(constants)? as u16;
            match op {
                Operation::Cbr => {
                    k = 0xff - k;
                }
                _ => {}
            }
            opcode |= (k & 0xf0) << 4 | (k & 0x0f);
        }
        Operation::Com
        | Operation::Neg
        | Operation::Inc
        | Operation::Dec
        | Operation::Push
        | Operation::Pop
        | Operation::Lsr
        | Operation::Ror
        | Operation::Asr
        | Operation::Swap => {
            let r = op_args[0].get_r8()?;
            opcode |= r.number() << 4;
        }
        Operation::Tst | Operation::Clr | Operation::Lsl | Operation::Rol => {
            let r = op_args[0].get_r8()?;
            opcode |= r.number() << 4;
            opcode |= (r.number() & 0x10) << 5 | r.number() & 0x0f;
        }
        Operation::Ser => {
            let r = op_args[0].get_r8()?;
            if r.number() < 16 {
                bail!("{:?} can only use a high register (r16 - r31)", op);
            }
            opcode |= (r.number() & 0x0f) << 4;
        }
        Operation::Muls => {
            let d = op_args[0].get_r8()?;
            if d.number() < 16 {
                bail!("{:?} can only use a high register (r16 - r31)", op);
            }
            opcode |= (d.number() & 0x0f) << 4;

            let r = op_args[1].get_r8()?;
            if r.number() < 16 {
                bail!("{:?} can only use a high register (r16 - r31)", op);
            }
            opcode |= r.number() & 0x0f;
        }
        Operation::Mulsu | Operation::Fmul | Operation::Fmuls | Operation::Fmulsu => {
            let d = op_args[0].get_r8()?;
            if d.number() < 16 {
                bail!("{:?} can only use registers (r16 - r23)", op);
            }
            opcode |= (d.number() & 0x07) << 4;

            let r = op_args[1].get_r8()?;
            if r.number() < 16 {
                bail!("{:?} can only use registers (r16 - r23)", op);
            }
            opcode |= r.number() & 0x07;
        }
        Operation::Rjmp | Operation::Rcall => {
            let k = op_args[0].get_expr()?;
            let k = k.run(constants)?;
            let rel = k - (current_address as i64 + 1);
            if rel < -2048 || rel > 2047 {
                bail!("Relative address out of range (-2048 <= k <= 2047)");
            }
            opcode |= (rel as u16) & 0x0fff;
        }
        Operation::Jmp | Operation::Call => {
            let k = op_args[0].get_expr()?;
            let k = k.run(constants)?;
            if k < 0 || k > 4194303 {
                bail!("Address out of range (0 <= k <= 4194303)");
            }

            let k = k as u64;
            opcode |= ((k & 0x3e0000) >> 13 | (k & 0x010000) >> 16) as u16;
            opcode_2part = (k & 0xffff) as u16;

            long_opcode = true;
        }
        Operation::Br(s) => {
            let mut index = 0;
            opcode |= match s {
                BranchT::Bs | BranchT::Bc => {
                    let s = op_args[index].get_expr()?;
                    index += 1;
                    s.get_bit_index(constants)? as u16
                }
                _ => 0,
            };
            let s = s.number();
            opcode |= s;

            let k = op_args[index].get_expr()?;
            let k = k.run(constants)?;
            let rel = k - (current_address as i64 + 1);
            if rel < -64 || rel > 63 {
                bail!("Relative address out of range (-64 <= k <= 63)");
            }
            opcode |= ((rel as u16) & 0x7f) << 3;
        }
        Operation::Movw => {
            let d = op_args[0].get_r8()?;
            if d.number() < 16 && d.number() % 2 != 0 {
                bail!("{:?} can only use a even numbered for Rd", op);
            }
            opcode |= (d.number() / 2) << 4;

            let r = op_args[1].get_r8()?;
            if r.number() < 16 && r.number() % 2 != 0 {
                bail!("{:?} can only use a even numbered for Rr", op);
            }
            opcode |= r.number() / 2;
        }

        Operation::Lds | Operation::Sts => {
            let (r, k) = if let Operation::Lds = op {
                (op_args[0].get_r8()?, op_args[1].get_expr()?)
            } else {
                (op_args[1].get_r8()?, op_args[0].get_expr()?)
            };
            opcode |= r.number() << 4;

            let k = k.run(constants)?;
            if k < 0 || k > 65535 {
                bail!("Address out of range (0 <= k <= 65535)");
            }

            opcode_2part = (k as u16) & 0xffff;

            long_opcode = true;
        }
        Operation::Ld | Operation::St | Operation::Ldd | Operation::Std => {
            let (r, i) = match op {
                Operation::Ld | Operation::Ldd => (op_args[0].get_r8()?, op_args[1].get_index()?),
                Operation::St | Operation::Std => (op_args[1].get_r8()?, op_args[0].get_index()?),
                _ => {
                    bail!("Unsupported variant!");
                }
            };
            opcode |= r.number() << 4;

            let reg_value = |i| match i {
                Reg16::X => 0b1100,
                Reg16::Y => 0b1000,
                Reg16::Z => 0b0000,
            };

            opcode |= match i {
                IndexOps::None(r16) => {
                    0b00 | if r16 == Reg16::X { 0x1000 } else { 0x0 } | reg_value(r16)
                }
                IndexOps::PostIncrement(r16) => 0b01 | 0x1000 | reg_value(r16),
                IndexOps::PreDecrement(r16) => 0b10 | 0x1000 | reg_value(r16),
                IndexOps::PostIncrementE(r16, expr) => {
                    let mut op = match r16 {
                        Reg16::X => {
                            bail!("Index regist must be Y or Z");
                        }
                        Reg16::Y => 0b1000,
                        Reg16::Z => 0b0000,
                    };
                    let k = expr.get_byte(constants)? as i8;
                    if k < 0 || k > 63 {
                        bail!("K out of range (0 <= P <= 63");
                    }
                    let k = k as u16;
                    op |= (k & 0x20) << 8 | (k & 0x18) << 7 | k & 0x07;
                    op
                }
            };
        }
        Operation::Lpm | Operation::Elpm => {
            if op_args.len() == 0 {
                opcode = if let Operation::Lpm = op {
                    0b_0101_1100_1000
                } else {
                    0b_0101_1101_1000
                }
            } else {
                let r = op_args[0].get_r8()?;
                opcode |= r.number() << 4;

                let i = op_args[1].get_index()?;

                opcode |= match i {
                    IndexOps::None(Reg16::Z) => 0b100,
                    IndexOps::PostIncrement(Reg16::Z) => 0b101,
                    _ => {
                        bail!("Other options are not allowed to lpm/elpm");
                    }
                };

                opcode |= if let Operation::Elpm = op { 0b10 } else { 0b0 };
            }
        }
        Operation::In | Operation::Out => {
            let (r, k) = if let Operation::In = op {
                (op_args[0].get_r8()?, op_args[1].get_expr()?)
            } else {
                (op_args[1].get_r8()?, op_args[0].get_expr()?)
            };
            opcode |= r.number() << 4;

            let k = k.get_byte(constants)? as i8;
            if k < 0 || k > 63 {
                bail!("I/O out of range (0 <= P <= 63");
            }
            let k = k as u16;
            opcode |= (k & 0x30) << 2 | k & 0x0f;
        }
        Operation::Bset | Operation::Bclr => {
            let k = op_args[0].get_expr()?;
            let k: u16 = k.get_bit_index(constants)? as u16;
            opcode |= k << 4;
        }
        Operation::Se(k) | Operation::Cl(k) => {
            let k = k.number();
            opcode |= k << 4;
        }
        _ => {}
    }
    // Write result opcode to finalized opcode
    let mut result = [0, 0];
    LittleEndian::write_u16(&mut result, opcode);
    finalized_opcode.extend(&result);

    // Write append part for long opcodes
    if long_opcode {
        let mut result = [0, 0];
        LittleEndian::write_u16(&mut result, opcode_2part);
        finalized_opcode.extend(&result);
    }

    Ok(finalized_opcode)
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::document::{document, Document};
    use crate::expr::{BinaryExpr, BinaryOperator, Expr};
    use crate::instruction::operation::{BranchT, Operation, SFlags};

    #[test]
    fn operation_test() {
        assert_eq!(document::operation("mov"), Ok(Operation::Mov));

        assert_eq!(document::operation("breq"), Ok(Operation::Br(BranchT::Eq)));

        assert_eq!(document::operation("sei"), Ok(Operation::Se(SFlags::I)));

        assert_eq!(document::operation("clt"), Ok(Operation::Cl(SFlags::T)));

        assert_eq!(
            document::operation("outi"),
            Ok(Operation::Custom("outi".to_string()))
        );

        assert_eq!(
            document::operation("brtt"),
            Ok(Operation::Custom("brtt".to_string()))
        );
    }

    #[test]
    fn reg8_test() {
        assert_eq!(document::reg8("r0"), Ok(Reg8::R0));

        assert_eq!(document::reg8("r21"), Ok(Reg8::R21));
    }

    #[test]
    fn reg16_test() {
        assert_eq!(document::reg16("X"), Ok(Reg16::X));

        assert_eq!(document::reg16("Y"), Ok(Reg16::Y));

        assert_eq!(document::reg16("Z"), Ok(Reg16::Z));
    }

    #[test]
    fn index_ops_test() {
        assert_eq!(document::index_ops("X"), Ok(IndexOps::None(Reg16::X)));

        assert_eq!(
            document::index_ops("-Y"),
            Ok(IndexOps::PreDecrement(Reg16::Y))
        );

        assert_eq!(
            document::index_ops("X+"),
            Ok(IndexOps::PostIncrement(Reg16::X))
        );

        assert_eq!(
            document::index_ops("Z+4"),
            Ok(IndexOps::PostIncrementE(Reg16::Z, Expr::Const(4)))
        );
    }

    #[test]
    fn instruction_ops_test() {
        assert_eq!(
            document::instruction_ops("r0"),
            Ok(InstructionOps::R8(Reg8::R0))
        );

        assert_eq!(
            document::instruction_ops("t"),
            Ok(InstructionOps::E(Expr::Ident("t".to_string())))
        );

        assert_eq!(
            document::instruction_ops("Y+24"),
            Ok(InstructionOps::Index(IndexOps::PostIncrementE(
                Reg16::Y,
                Expr::Const(24)
            )))
        );
    }

    #[test]
    fn op_list_test() {
        assert_eq!(document::op_list(""), Ok(vec![]));

        assert_eq!(
            document::op_list("  a\t, b,c  ,\td"),
            Ok(vec![
                InstructionOps::E(Expr::Ident("a".to_string())),
                InstructionOps::E(Expr::Ident("b".to_string())),
                InstructionOps::E(Expr::Ident("c".to_string())),
                InstructionOps::E(Expr::Ident("d".to_string())),
            ])
        );

        assert_eq!(
            document::op_list("a,b,c,d"),
            Ok(vec![
                InstructionOps::E(Expr::Ident("a".to_string())),
                InstructionOps::E(Expr::Ident("b".to_string())),
                InstructionOps::E(Expr::Ident("c".to_string())),
                InstructionOps::E(Expr::Ident("d".to_string())),
            ])
        );
    }

    #[test]
    fn instruction_line_test() {
        assert_eq!(
            document::instruction_line("cli"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Cl(SFlags::I),
                vec![]
            ))
        );

        assert_eq!(
            document::instruction_line("push r0"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Push,
                vec![InstructionOps::R8(Reg8::R0)]
            ))
        );

        assert_eq!(
            document::instruction_line("ldi r16, 1 << 4 | 1 << 2"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Ldi,
                vec![
                    InstructionOps::R8(Reg8::R16),
                    InstructionOps::E(Expr::Binary(Box::new(BinaryExpr {
                        left: Expr::Binary(Box::new(BinaryExpr {
                            left: Expr::Const(1),
                            operator: BinaryOperator::ShiftLeft,
                            right: Expr::Const(4)
                        })),
                        operator: BinaryOperator::Or,
                        right: Expr::Binary(Box::new(BinaryExpr {
                            left: Expr::Const(1),
                            operator: BinaryOperator::ShiftLeft,
                            right: Expr::Const(2)
                        }))
                    })))
                ]
            ))
        );

        assert_eq!(
            document::instruction_line("ldi r18, high(t)"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Ldi,
                vec![
                    InstructionOps::R8(Reg8::R18),
                    InstructionOps::E(Expr::Func(
                        Box::new(Expr::Ident("high".to_string())),
                        Box::new(Expr::Ident("t".to_string()))
                    ))
                ]
            ))
        );

        assert_eq!(
            document::instruction_line("ld r19, X+"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Ld,
                vec![
                    InstructionOps::R8(Reg8::R19),
                    InstructionOps::Index(IndexOps::PostIncrement(Reg16::X))
                ]
            ))
        );

        assert_eq!(
            document::instruction_line("outi UDR0, r16, 1 << 4 | 1 << 2"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Custom("outi".to_string()),
                vec![
                    InstructionOps::E(Expr::Ident("UDR0".to_string())),
                    InstructionOps::R8(Reg8::R16),
                    InstructionOps::E(Expr::Binary(Box::new(BinaryExpr {
                        left: Expr::Binary(Box::new(BinaryExpr {
                            left: Expr::Const(1),
                            operator: BinaryOperator::ShiftLeft,
                            right: Expr::Const(4)
                        })),
                        operator: BinaryOperator::Or,
                        right: Expr::Binary(Box::new(BinaryExpr {
                            left: Expr::Const(1),
                            operator: BinaryOperator::ShiftLeft,
                            right: Expr::Const(2)
                        }))
                    })))
                ]
            ))
        );

        assert_eq!(
            document::instruction_line("sei ; enable interrupts"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Se(SFlags::I),
                vec![]
            ))
        );

        assert_eq!(
            document::instruction_line("exit: ret"),
            Ok(Document::CodeLine(
                Box::new(Some(Document::Label("exit".to_string()))),
                Operation::Ret,
                vec![]
            ))
        );

        assert_eq!(
            document::instruction_line("fail: reti; exit from interrupt"),
            Ok(Document::CodeLine(
                Box::new(Some(Document::Label("fail".to_string()))),
                Operation::Reti,
                vec![]
            ))
        );
    }
}
