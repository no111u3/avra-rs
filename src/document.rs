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

        // no empty spaces
        rule ne_space() = [' ' | '\t']+

        // new line
        rule new_line() = ['\n' | '\r']*

        rule char_ident() = ['0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ]

        // ident
        pub rule ident() -> Document
            = i:$(['a'..='z' | 'A'..='Z' | '_' ] char_ident()*) {
            Document::Ident(i.parse().unwrap())
        }

        // label
        pub rule label() -> Document
            = l:$(ident())":" {
            Document::Label(l.to_lowercase())
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
            = i:$(['a'..='z' | 'A'..='Z' | '_' ]['0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ]*) {
            Expr::Ident(i.parse().unwrap())
        }

        // expression const
        pub rule e_const() -> Expr
            = "$" n:$(['0'..='9' | 'A'..='F' | 'a'..='f']+) { Expr::Const(i64::from_str_radix(n, 16).unwrap()) }
            / "0x" n:$(['0'..='9' | 'A'..='F' | 'a'..='f']+) { Expr::Const(i64::from_str_radix(n, 16).unwrap()) }
            / "0b" n:$(['0'..='1']+) { Expr::Const(i64::from_str_radix(n, 2).unwrap()) }
            / "0" n:$(['0'..='7']+) { Expr::Const(i64::from_str_radix(n, 8).unwrap()) }
            / n:$(['0'..='9']+) { Expr::Const(n.parse().unwrap()) }

        // expression
        pub rule expr() -> Expr
            = precedence! {
            // precedence 4
            x:(@) space() "||" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::LogicalOr, right: y})) }
            --
            // precedence 5
            x:(@) space() "&&" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::LogicalAnd, right: y})) }
            --
            // precedence 6
            x:(@) space() "|" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::BitwiseOr, right: y})) }
            --
            // precedence 7
            x:(@) space() "^" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::BitwiseXor, right: y})) }
            --
            // precedence 8
            x:(@) space() "&" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::BitwiseAnd, right: y})) }
            --
            // precedence 9
            x:(@) space() "==" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Equal, right: y})) }
            x:(@) space() "!=" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::NotEqual, right: y})) }
            --
            // precedence 10
            x:(@) space() "<" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::LessThan, right: y})) }
            x:(@) space() "<=" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::LessOrEqual, right: y})) }
            x:(@) space() ">" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::GreaterThan, right: y})) }
            x:(@) space() ">=" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::GreaterOrEqual, right: y})) }
            --
            // precedence 11
            x:(@) space() "<<" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::ShiftLeft, right: y})) }
            x:(@) space() ">>" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::ShiftRight, right: y})) }
            --
            // precedence 12
            x:(@) space() "+" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Add, right: y})) }
            x:(@) space() "-" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Sub, right: y})) }
            "~" v:@ { Expr::Unary(Box::new(UnaryExpr{operator: UnaryOperator::Not, expr: v})) }
            --
            // precedence 13
            x:(@) space() "*" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Mul, right: y})) }
            x:(@) space() "/" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Div, right: y})) }
            x:(@) space() "%" space() y:@ { Expr::Binary(Box::new(BinaryExpr{left: x, operator: BinaryOperator::Rem, right: y})) }
            --
            // precedence 14
            "-" v:@ { Expr::Unary(Box::new(UnaryExpr{operator: UnaryOperator::Minus, expr: v})) }
            --
            // precedence 15
            n:e_ident() space() "(" space() args:expr() space() ")" { Expr::Func(Box::new(n), Box::new(args)) }
            "(" space() be:expr() space() ")" { be }
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
                if let Ok(op) = document::standard_operation(op_name.to_lowercase().as_str()) {
                    op
                } else {
                    Operation::Custom(op_name.to_lowercase())
                }
            }

        pub rule reg8() -> Reg8
            = r_name:$(['r' | 'R'] ['0'..='9']*<1,2>) { Reg8::from_str(r_name.to_lowercase().as_str()).unwrap() }

        pub rule reg16() -> Reg16
            = r_name:$(['x' | 'y' | 'z' | 'X' | 'Y' | 'Z']) { Reg16::from_str(r_name.to_lowercase().as_str()).unwrap() }

        pub rule index_ops() -> IndexOps
            = "-" r:reg16() { IndexOps::PreDecrement(r) }
            / r:reg16() "+" e:expr() { IndexOps::PostIncrementE(r, e) }
            / r:reg16() "+" { IndexOps::PostIncrement(r) }
            / r:reg16() !char_ident() { IndexOps::None(r) }


        pub rule instruction_ops() -> InstructionOps
            = ind:index_ops() { InstructionOps::Index(ind) }
            / r8:reg8() { InstructionOps::R8(r8) }
            / e:expr() { InstructionOps::E(e) }


        pub rule standard_directive() -> Result<Directive, strum::ParseError>
            = d:$(['a'..='z']+) { Directive::from_str(d) }

        pub rule directive() -> Directive
            = ("." / "#" ) d_name:$(['a'..='z']+) {
            if let Ok(Ok(d)) = document::standard_directive(d_name.to_lowercase().as_str()) {
                d
            } else {
                Directive::Custom(d_name.to_string())
            }
        }

        rule delimiter()
            = space() "," space()

        // operands list
        pub rule op_list() -> Vec<InstructionOps>
            = instruction_ops() ** delimiter()

        // comment
        rule asm_comment() = ";" [_]* new_line()

        rule c_comment() = "/*" (!("*/" / "\n" / "\r") [_])* "*/" new_line()

        rule c_another_comment() = "//" [_]* new_line()

        pub rule comment()
            = asm_comment()
            / c_comment()
            / c_another_comment()

        // instruction line
        pub rule instruction_line() -> Document
            = l:label()? space() o:operation() space() ol:op_list() space() comment()? {Document::CodeLine(Box::new(l), o, ol)}

        // directive operand
        pub rule directive_op() ->  Operand
            = e:expr() { Operand::E(e) }
            / s:string() { Operand::S(s) }

        rule delimiter_space()
            = ne_space() space()

        // directive operands
        pub rule directive_ops() -> DirectiveOps
            = a:e_ident() space() "=" space() e:expr() { DirectiveOps::Assign(a, e) }
            // Start pragma hack
            / a:directive_op() ne_space() b:directive_op() ne_space() c:directive_op() ne_space() d:directive_op() ne_space() e:directive_op() ne_space() f:directive_op() { DirectiveOps::OpList(vec![a, b, c, d, e, f]) }
            / a:directive_op() ne_space() b:directive_op() ne_space() c:directive_op() ne_space() d:directive_op() ne_space() e:directive_op() { DirectiveOps::OpList(vec![a, b, c, d, e]) }
            / a:directive_op() ne_space() b:directive_op() ne_space() c:directive_op() ne_space() d:directive_op() { DirectiveOps::OpList(vec![a, b, c, d]) }
            / a:directive_op() ne_space() b:directive_op() ne_space() c:directive_op() { DirectiveOps::OpList(vec![a, b, c]) }
            / a:directive_op() ne_space() b:directive_op() { DirectiveOps::OpList(vec![a, b]) }
            // End pragma hack
            / ol: directive_op() ** delimiter() { DirectiveOps::OpList(ol) }

        // directive line
        pub rule directive_line() -> Document
            = l:label()? space() d:directive() space() os:directive_ops() space() comment()? { Document::DirectiveLine(Box::new(l), d, os) }

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
                        operator: BinaryOperator::BitwiseOr,
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
                        operator: BinaryOperator::BitwiseOr,
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
                Directive::DSeg,
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
