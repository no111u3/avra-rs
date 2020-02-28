//! Contains rules for document parsing of AVRA-rs

use std::str::FromStr;

use peg::parser;

use crate::directive::{Directive, DirectiveOps, Operand};
use crate::expr::{BinaryExpr, BinaryOperator, Expr, UnaryExpr, UnaryOperator};
use crate::instruction::{
    operation::{BranchT, Operation, SFlags},
    register::{Reg16, Reg8},
    IndexOps, InstructionOps,
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Document {
    EmptyLine,
    Ident(String),
    Label(String),
    CodeLine(Box<Option<Document>>, Operation, Vec<InstructionOps>),
    DirectiveLine(Box<Option<Document>>, Directive, DirectiveOps),
}

parser! {
    pub grammar document() for str {
        // spaces
        rule space() = [' ' | '\t']*

        // new line
        rule new_line() = ['\n' | '\r']*

        // ident
        pub rule ident() -> Document
            = i:$(['a'..='z' | 'A'..='Z' | '_' ]['0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ]*) {
            Document::Ident(i.parse().unwrap())
        }

        // label
        pub rule label() -> Document
            = l:$(ident())":" {
            Document::Label(l.to_string())
        }

        // string
        // note: escaped chars not handled
        pub rule string() -> String
            = "\"" s:$((!("\"" / "\n" / "\r") [_])*) "\"" { s.to_string() }

        // char
        // note: escaped chars not handled
        pub rule ch() -> char
            = "'" c:$((!("'" / "\n" / "\r") [_])*<1>) "'" { c.chars().next().unwrap() }


        // expression ident
        pub rule e_ident() -> Expr
            = space() i:$(['a'..='z' | 'A'..='Z' | '_' ]['0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ]*) space() {
            Expr::Ident(i.parse().unwrap())
        }

        // expression const
        pub rule e_const() -> Expr
            = space() "$" n:$(['0'..='9' | 'A'..='F' | 'a'..='f']+) space() { Expr::Const(i64::from_str_radix(n, 16).unwrap()) }
            / space() "0x" n:$(['0'..='9' | 'A'..='F' | 'a'..='f']+) space() { Expr::Const(i64::from_str_radix(n, 16).unwrap()) }
            / space() "0b" n:$(['0'..='1']+) space() { Expr::Const(i64::from_str_radix(n, 2).unwrap()) }
            / space() "0" n:$(['0'..='7']+) space() { Expr::Const(i64::from_str_radix(n, 8).unwrap()) }
            / space() n:$(['0'..='9']+) space() { Expr::Const(n.parse().unwrap()) }

        // expression
        pub rule expr() -> Expr
            = precedence! {
            // precedence 6
            x:(@) "|" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Or, right: y})) }
            --
            // precedence 7
            x:(@) "^" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Xor, right: y})) }
            --
            // precedence 8
            x:(@) "&" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::And, right: y})) }
            --
            // precedence 11
            x:(@) "<<" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::ShiftLeft, right: y})) }
            x:(@) ">>" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::ShiftRight, right: y})) }
            --
            // precedence 12
            x:(@) "+" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Add, right: y})) }
            x:(@) "-" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Sub, right: y})) }
            "~" v:@ { Expr::Unary(Box::new(UnaryExpr{operator: UnaryOperator::Not, expr: v})) }
            --
            // precedence 13
            x:(@) "*" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Mul, right: y})) }
            x:(@) "/" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Div, right: y})) }
            x:(@) "%" y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Rem, right: y})) }
            --
            // precedence 14
            "-" v:@ { Expr::Unary(Box::new(UnaryExpr{operator: UnaryOperator::Minus, expr: v})) }
            --
            // precedence 15
            n:e_ident() space() "(" args:expr() ")" { Expr::Func(Box::new(n), Box::new(args)) }
            space() "(" be:expr() ")" space() { be }
            c:e_const() { c }
            c:ch() { Expr::Const(c as i64) }
            i:e_ident() { i }
        }

        rule branc_op() -> &'input str
            = $("eq" / "ne" / "cs" / "cc"
            / "sh" / "lo" / "mi" / "pl"
            / "ge" / "lt" / "hs" / "hc"
            / "ts" / "tc" / "vs" / "vc"
            / "ie" / "id" / "bs" / "bc")

        rule flag_op() -> &'input str
            = $("c" / "z" / "n" / "v" / "s" / "h" / "t" / "i")

        rule op() -> &'input str
            = $("wdr" / "tst" / "swap" / "subi" / "sub" / "sts" / "std" / "st" / "spm"
            / "sleep" / "ser" / "sbr" / "sbiw" / "sbci" / "sbc" / "ror" / "rol" / "rjmp"
            / "reti" / "ret" / "rcall" / "push" / "pop" / "out" / "ori" / "or" / "nop"
            / "neg" / "mulsu" / "muls" / "mul" / "movw" / "mov" / "lsr" / "lsl" / "lpm"
            / "lds" / "ldi" / "ldd" / "ld" / "jmp" / "inc" / "in" / "ijmp" / "icall"
            / "fmulsu" / "fmuls" / "fmul" / "eor" / "elpm" / "eijmp" / "eicall" / "dec"
            / "cpse" / "cpi" / "cpc" / "cp" / "com" / "clr" / "cbr" / "call" / "bset"
            / "break" / "bclr" / "asr" / "andi" / "and" / "adiw" / "add" / "adc")

        pub rule standard_operation() -> Operation
            = "br" br_type:branc_op() { Operation::Br(BranchT::from_str(br_type).unwrap()) }
            / "se" se_type:flag_op() { Operation::Se(SFlags::from_str(se_type).unwrap()) }
            / "cl" cl_type:flag_op() { Operation::Cl(SFlags::from_str(cl_type).unwrap()) }
            / op_name:op() { Operation::from_str(op_name).unwrap() }

        pub rule operation() -> Operation
            = op_name:$(['a'..='z']+) {
                if let Ok(op) = document::standard_operation(op_name) {
                    op
                } else {
                    Operation::Custom(op_name.to_string())
                }
            }

        pub rule reg8() -> Reg8
            = r_name:$("r" ['0'..='9']*<1,2>) { Reg8::from_str(r_name).unwrap() }

        pub rule reg16() -> Reg16
            = r_name:$(['X' | 'Y' | 'Z']) { Reg16::from_str(r_name).unwrap() }

        pub rule index_ops() -> IndexOps
            = "-" r:reg16() { IndexOps::PreDecrement(r) }
            / r:reg16() "+" e:expr() { IndexOps::PostIncrementE(r, e) }
            / r:reg16() "+" { IndexOps::PostIncrement(r) }
            / r:reg16() { IndexOps::None(r) }

        pub rule instruction_ops() -> InstructionOps
            = space() ind:index_ops() space() { InstructionOps::Index(ind) }
            / space() r8:reg8() space() { InstructionOps::R8(r8) }
            / e:expr() { InstructionOps::E(e) }

        pub rule standard_directive() -> Result<Directive, strum::ParseError>
            = d:$(['a'..='z']+) { Directive::from_str(d) }

        pub rule directive() -> Directive
            = ("." / "#" ) d_name:$(['a'..='z']+) {
            if let Ok(Ok(d)) = document::standard_directive(d_name) {
                d
            } else {
                Directive::Custom(d_name.to_string())
            }
        }

        // operands list
        pub rule op_list() -> Vec<InstructionOps>
            = instruction_ops() ** ","

        // comment
        pub rule comment() = ";" [_]* new_line()

        // instruction line
        pub rule instruction_line() -> Document
            = l:label()? space() o:operation() space() ol:op_list() comment()? {Document::CodeLine(Box::new(l), o, ol)}

        // directive operand
        pub rule directive_op() ->  Operand
            = e:expr() { Operand::E(e) }
            / space() s:string() space() { Operand::S(s) }

        // directive operands
        pub rule directive_ops() -> DirectiveOps
            = a:e_ident() "=" e:expr() { DirectiveOps::Assign(a, e) }
            / ol: directive_op() ** "," { DirectiveOps::OpList(ol) }

        // directive line
        pub rule directive_line() -> Document
            = l:label()? space() d:directive() space() os:directive_ops() comment()? { Document::DirectiveLine(Box::new(l), d, os) }

        // line
        pub rule line() -> Document
            = d_l:directive_line() { d_l }
            / i_l:instruction_line() { i_l }
            / l:label() space() comment()? { l }
            / space() comment() { Document::EmptyLine }
            / space() new_line() { Document::EmptyLine }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn ident_test() {
        assert_eq!(document::ident("_"), Ok(Document::Ident("_".to_string())));

        assert_eq!(document::ident("l"), Ok(Document::Ident("l".to_string())));

        assert_eq!(
            document::ident("Test_string_of_ident"),
            Ok(Document::Ident("Test_string_of_ident".to_string()))
        );

        assert!(document::ident("4").is_err());
    }

    #[test]
    fn label_test() {
        assert_eq!(document::label("_:"), Ok(Document::Label("_".to_string())));

        assert_eq!(
            document::label("test_label:"),
            Ok(Document::Label("test_label".to_string()))
        );

        assert!(document::ident("4").is_err());

        assert!(document::ident("4:").is_err());
    }

    #[test]
    fn string_test() {
        assert_eq!(document::string("\"\""), Ok("".to_string()));

        assert_eq!(document::string("\"X\""), Ok("X".to_string()));

        assert_eq!(
            document::string("\"Bla bla bla x,jljlsdfsfsdf//()_\t\""),
            Ok("Bla bla bla x,jljlsdfsfsdf//()_\t".to_string())
        );
    }

    #[test]
    fn ch_test() {
        assert_eq!(document::ch("'x'"), Ok('x'));
    }

    #[test]
    fn comment_test() {
        assert_eq!(document::comment(";"), Ok(()));

        assert_eq!(document::comment("; "), Ok(()));

        assert_eq!(document::comment("; bla bla bla"), Ok(()));

        assert_eq!(document::comment("; bla bla bla\n"), Ok(()));
    }

    #[test]
    fn line_test() {
        assert_eq!(document::line(""), Ok(Document::EmptyLine));

        assert_eq!(document::line("\n"), Ok(Document::EmptyLine));

        assert_eq!(
            document::line("\t\t; this is more comment\n"),
            Ok(Document::EmptyLine)
        );

        assert_eq!(
            document::line("test_label:"),
            Ok(Document::Label("test_label".to_string()))
        );

        assert_eq!(
            document::line("test_label: ; with comment"),
            Ok(Document::Label("test_label".to_string()))
        );

        assert_eq!(
            document::line("cli"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Cl(SFlags::I),
                vec![]
            ))
        );

        assert_eq!(
            document::line("push r0"),
            Ok(Document::CodeLine(
                Box::new(None),
                Operation::Push,
                vec![InstructionOps::R8(Reg8::R0)]
            ))
        );

        assert_eq!(
            document::line("ldi r16, 1 << 4 | 1 << 2"),
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
            document::line("ldi r18, high(t)"),
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
            document::line("ld r19, X+"),
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
            document::line("outi UDR0, r16, 1 << 4 | 1 << 2"),
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
            document::line("fail: reti; exit from interrupt"),
            Ok(Document::CodeLine(
                Box::new(Some(Document::Label("fail".to_string()))),
                Operation::Reti,
                vec![]
            ))
        );

        assert_eq!(
            document::line(".dseg"),
            Ok(Document::DirectiveLine(
                Box::new(None),
                Directive::Dseg,
                DirectiveOps::OpList(vec![])
            ))
        );

        assert_eq!(
            document::line(".equ Last = 8"),
            Ok(Document::DirectiveLine(
                Box::new(None),
                Directive::Equ,
                DirectiveOps::Assign(Expr::Ident("Last".to_string()), Expr::Const(8))
            ))
        );
    }
}
