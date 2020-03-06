//! Contains expression representation of AVRA-rs

use byteorder::{ByteOrder, LittleEndian};
use failure::Fail;

use crate::context::Context;

/// Assembly uses constant expressions to avoid copying magic numbers around.
/// Expr represents these constant expressions.
///
/// The run method evaluates the constant expression.
/// The get_2bytes, get_byte and get_bit_index evaluate the constant expression but also convert
/// to a specific low level type needed by instructions.

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Ident(String),
    Const(i64),
    Func(Box<Expr>, Box<Expr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
}

impl Expr {
    pub fn binary(left: Expr, operator: BinaryOperator, right: Expr) -> Expr {
        Expr::Binary(Box::new(BinaryExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn unary(expr: Expr, operator: UnaryOperator) -> Expr {
        Expr::Unary(Box::new(UnaryExpr { expr, operator }))
    }

    pub fn get_words(&self, constants: &dyn Context) -> Result<[u8; 2], ExprRunError> {
        let value = self.run(constants)?;
        if value > 0xFFFF || value < -32768 {
            Err(ExprRunError::ResultDoesntFit(format!(
                "0x{} > 0xFFFF This is invalid because the value needs to fit in word",
                value
            )))
        } else {
            let mut result = [0, 0];
            LittleEndian::write_u16(&mut result, value as u16);
            Ok(result)
        }
    }

    pub fn get_byte(&self, constants: &dyn Context) -> Result<u8, ExprRunError> {
        let value = self.run(constants)?;
        if value > 0xFF || value < -128 {
            Err(ExprRunError::ResultDoesntFit(format!(
                "0x{:x} > 0xFF This is invalid because the value needs to fit in one byte",
                value
            )))
        } else {
            Ok(value as u8)
        }
    }

    pub fn get_bit_index(&self, constants: &dyn Context) -> Result<u8, ExprRunError> {
        let value = self.run(constants)?;
        if value > 7 {
            Err(ExprRunError::ResultDoesntFit(format!(
                "{} > 7 This is invalid because the value needs to index bits in a byte.",
                value
            )))
        } else {
            Ok(value as u8)
        }
    }

    pub fn run(&self, constants: &dyn Context) -> Result<i64, ExprRunError> {
        match self {
            Expr::Ident(ident) => match constants.get_expr(ident) {
                Some(Expr::Const(address)) => Ok(address),
                // TODO: check recursion for cross linked equs and other labels
                Some(expr) => expr.run(constants),
                None => Err(ExprRunError::MissingIdentifier(ident.clone())),
            },
            Expr::Const(value) => Ok(*value),
            Expr::Func(ident, argument) => {
                if let Expr::Ident(name) = &**ident {
                    let value = argument.run(constants)?;
                    let ret_val = match name.to_lowercase().as_str() {
                        "low" => (value as u64 & 0xff) as i64,
                        "high" | "byte2" => ((value as u64 & 0xff00) >> 8) as i64,
                        "byte3" => ((value as u64 & 0xff0000) >> 16) as i64,
                        "byte4" => ((value as u64 & 0xff000000) >> 24) as i64,
                        "lwrd" => (value as u64 & 0xffff) as i64,
                        "hwrd" => ((value as u64 & 0xffff0000) >> 16) as i64,
                        "page" => ((value as u64 & 0x1f0000) >> 16) as i64,
                        "exp2" => 1 << value,
                        "log2" => {
                            let mut i = 0;
                            let mut value = value as u64;
                            while value > 0 {
                                value >>= 1;
                                i += 1
                            }
                            i
                        }
                        _ => return Err(ExprRunError::MissingFunction(name.clone())),
                    };

                    Ok(ret_val)
                } else {
                    Err(ExprRunError::ResultDoesntFit(format!(
                        "function name must be Ident, get: {:?}",
                        ident
                    )))
                }
            }
            Expr::Binary(binary) => {
                let left = binary.left.run(constants)?;
                let right = binary.right.run(constants)?;
                match binary.operator {
                    BinaryOperator::Add => match left.checked_add(right) {
                        Some(value) => Ok(value),
                        None => Err(ExprRunError::ArithmeticError(format!(
                            "Addition overflowed: {:?} + {:?}",
                            binary.left, binary.right
                        ))),
                    },
                    BinaryOperator::Sub => match left.checked_sub(right) {
                        Some(value) => Ok(value),
                        None => Err(ExprRunError::ArithmeticError(format!(
                            "Subtraction underflowed: {:?} - {:?}",
                            binary.left, binary.right
                        ))),
                    },
                    BinaryOperator::Mul => match left.checked_mul(right) {
                        Some(value) => Ok(value),
                        None => Err(ExprRunError::ArithmeticError(format!(
                            "Multiplication overflowed: {:?} * {:?}",
                            binary.left, binary.right
                        ))),
                    },
                    BinaryOperator::Div => {
                        if right == 0 {
                            Err(ExprRunError::ArithmeticError(format!(
                                "Attempted to divide by zero: {:?} / {:?}",
                                binary.left, binary.right
                            )))
                        } else {
                            match left.checked_div(right) {
                                Some(value) => Ok(value),
                                None => Err(ExprRunError::ArithmeticError(format!(
                                    "Division overflowed: {:?} / {:?}",
                                    binary.left, binary.right
                                ))),
                            }
                        }
                    }
                    BinaryOperator::Rem => {
                        if right == 0 {
                            Err(ExprRunError::ArithmeticError(format!(
                                "Attempted to divide by zero (remainder): {:?} % {:?}",
                                binary.left, binary.right
                            )))
                        } else {
                            match left.checked_rem(right) {
                                Some(value) => Ok(value),
                                None => Err(ExprRunError::ArithmeticError(format!(
                                    "Remainder overflowed: {:?} % {:?}",
                                    binary.left, binary.right
                                ))),
                            }
                        }
                    }
                    BinaryOperator::And => Ok(left & right),
                    BinaryOperator::Or => Ok(left | right),
                    BinaryOperator::Xor => Ok(left ^ right),
                    BinaryOperator::ShiftLeft => Ok(left << right),
                    BinaryOperator::ShiftRight => Ok(left >> right),
                }
            }
            Expr::Unary(unary) => match unary.operator {
                UnaryOperator::Minus => {
                    let value = unary.expr.run(constants)?;
                    match value.checked_neg() {
                        Some(value) => Ok(value),
                        None => Err(ExprRunError::ArithmeticError(format!(
                            "Failed to get negative value of: {}",
                            value
                        ))),
                    }
                }
                UnaryOperator::Not => {
                    let value = unary.expr.run(constants)?;
                    Ok(value.reverse_bits())
                }
            },
        }
    }
}

#[derive(Debug, Fail)]
pub enum ExprRunError {
    #[fail(display = "Function {} can not be found.", _0)]
    MissingFunction(String),
    #[fail(display = "Identifier {} can not be found.", _0)]
    MissingIdentifier(String),
    #[fail(display = "Arithmetic error: {}", _0)]
    ArithmeticError(String),
    #[fail(display = "{}", _0)]
    ResultDoesntFit(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub expr: Expr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Xor,
    Or,
    ShiftLeft,
    ShiftRight,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::document::document;

    #[test]
    fn e_ident_test() {
        assert_eq!(document::e_ident("_"), Ok(Expr::Ident("_".to_string())));

        assert_eq!(document::e_ident("l"), Ok(Expr::Ident("l".to_string())));

        assert_eq!(
            document::e_ident("Test_string_of_ident"),
            Ok(Expr::Ident("Test_string_of_ident".to_string()))
        );

        assert!(document::e_ident("4").is_err());
    }

    #[test]
    fn e_const_test() {
        assert_eq!(document::e_const("4"), Ok(Expr::Const(4)));

        assert_eq!(document::e_const("0x3519"), Ok(Expr::Const(0x3519)));

        assert_eq!(document::e_const("$4a"), Ok(Expr::Const(0x4a)));

        assert_eq!(document::e_const("03516"), Ok(Expr::Const(0o3516)));

        assert_eq!(document::e_const("0b110100"), Ok(Expr::Const(0b110100)));

        assert!(document::e_const("4a").is_err());

        assert!(document::e_const("0x4z").is_err());

        assert!(document::e_const("$4y").is_err());

        assert!(document::e_const("0489").is_err());

        assert!(document::e_const("0b2103").is_err());
    }

    #[test]
    fn expr_test() {
        assert_eq!(
            document::expr("Test_string_of_ident"),
            Ok(Expr::Ident("Test_string_of_ident".to_string()))
        );

        assert_eq!(document::expr("03516"), Ok(Expr::Const(0o3516)));

        assert_eq!(document::expr("' '"), Ok(Expr::Const(0x20)));

        assert_eq!(document::expr("(t)"), Ok(Expr::Ident("t".to_string())));

        assert_eq!(
            document::expr("high(t)"),
            Ok(Expr::Func(
                Box::new(Expr::Ident("high".to_string())),
                Box::new(Expr::Ident("t".to_string()))
            ))
        );

        assert_eq!(
            document::expr("1 << 2 | 1 << 1"),
            Ok(Expr::Binary(Box::new(BinaryExpr {
                left: Expr::Binary(Box::new(BinaryExpr {
                    left: Expr::Const(1),
                    operator: BinaryOperator::ShiftLeft,
                    right: Expr::Const(2),
                })),
                operator: BinaryOperator::Or,
                right: Expr::Binary(Box::new(BinaryExpr {
                    left: Expr::Const(1),
                    operator: BinaryOperator::ShiftLeft,
                    right: Expr::Const(1),
                })),
            })))
        );
    }
}
