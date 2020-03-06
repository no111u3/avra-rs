use crate::expr::Expr;
use crate::instruction::register::Reg8;
use crate::parser::SegmentType;

pub trait Context {
    fn get_define(&self, _name: &String) -> Option<Expr> {
        None
    }
    fn get_equ(&self, _name: &String) -> Option<Expr> {
        None
    }
    fn get_label(&self, _name: &String) -> Option<(SegmentType, u32)> {
        None
    }
    fn get_def(&self, _name: &String) -> Option<Reg8> {
        None
    }
    fn get_set(&self, _name: &String) -> Option<Expr> {
        None
    }

    fn set_define(&self, _name: String, _value: Expr) -> Option<Expr> {
        None
    }
    fn set_equ(&self, _name: String, _value: Expr) -> Option<Expr> {
        None
    }
    fn set_label(&self, _name: String, _value: (SegmentType, u32)) -> Option<(SegmentType, u32)> {
        None
    }
    fn set_def(&self, _name: String, _value: Reg8) -> Option<Reg8> {
        None
    }

    fn get_expr(&self, name: &String) -> Option<Expr> {
        if let Some(expr) = self.get_define(name) {
            Some(expr)
        } else if let Some(expr) = self.get_equ(name) {
            Some(expr)
        } else if let Some(expr) = self.get_set(name) {
            Some(expr)
        } else {
            self.get_label(name).map(|x| Expr::Const(x.1 as i64))
        }
    }

    fn exist(&self, name: &String) -> bool {
        if let Some(_) = self.get_expr(name) {
            true
        } else if let Some(_) = self.get_label(name) {
            true
        } else if let Some(_) = self.get_def(name) {
            true
        } else {
            false
        }
    }
}
