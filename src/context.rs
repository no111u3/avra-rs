use crate::device::Device;
use crate::expr::Expr;
use crate::instruction::register::Reg8;
use crate::parser::SegmentType;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use maplit::hashmap;

pub trait Context {
    fn get_define(&self, _name: &String) -> Option<Expr>;
    fn get_equ(&self, _name: &String) -> Option<Expr>;
    fn get_label(&self, _name: &String) -> Option<(SegmentType, u32)>;
    fn get_def(&self, _name: &String) -> Option<Reg8>;
    fn get_set(&self, _name: &String) -> Option<Expr>;
    fn get_special(&self, _name: &String) -> Option<Expr>;
    fn get_device(&self) -> Device;

    fn set_define(&self, _name: String, _value: Expr) -> Option<Expr>;
    fn set_equ(&self, _name: String, _value: Expr) -> Option<Expr>;
    fn set_label(&self, _name: String, _value: (SegmentType, u32)) -> Option<(SegmentType, u32)>;
    fn set_def(&self, _name: String, _value: Reg8) -> Option<Reg8>;
    fn set_special(&self, _name: String, _value: Expr) -> Option<Expr>;

    fn get_expr(&self, name: &String) -> Option<Expr> {
        if let Some(expr) = self.get_define(name) {
            Some(expr)
        } else if let Some(expr) = self.get_equ(name) {
            Some(expr)
        } else if let Some(expr) = self.get_set(name) {
            Some(expr)
        } else if let Some(expr) = self.get_special(name) {
            Some(expr)
        } else {
            self.get_label(name).map(|x| Expr::Const(x.1 as i64))
        }
    }

    fn exist(&self, name: &String) -> bool {
        if let Some(_) = self.get_expr(name) {
            true
        } else if let Some(_) = self.get_def(name) {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CommonContext {
    // defines
    pub defines: Rc<RefCell<HashMap<String, Expr>>>,
    // equals
    pub equs: Rc<RefCell<HashMap<String, Expr>>>,
    // labels
    pub labels: Rc<RefCell<HashMap<String, (SegmentType, u32)>>>,
    // defs
    pub defs: Rc<RefCell<HashMap<String, Reg8>>>,
    // sets
    pub sets: Rc<RefCell<HashMap<String, Expr>>>,
    // special
    pub special: Rc<RefCell<HashMap<String, Expr>>>,
    // device
    pub device: Rc<RefCell<Option<Device>>>,
}

impl CommonContext {
    pub fn new() -> Self {
        Self {
            defines: Rc::new(RefCell::new(hashmap! {})),
            equs: Rc::new(RefCell::new(hashmap! {})),
            labels: Rc::new(RefCell::new(hashmap! {})),
            defs: Rc::new(RefCell::new(hashmap! {})),
            sets: Rc::new(RefCell::new(hashmap! {})),
            special: Rc::new(RefCell::new(hashmap! {})),
            device: Rc::new(RefCell::new(Some(Device::new(0)))),
        }
    }
}

impl Context for CommonContext {
    fn get_define(&self, name: &String) -> Option<Expr> {
        self.defines.borrow().get(name).map(|x| x.clone())
    }

    fn get_equ(&self, name: &String) -> Option<Expr> {
        self.equs
            .borrow()
            .get(&name.to_lowercase())
            .map(|x| x.clone())
    }

    fn get_label(&self, name: &String) -> Option<(SegmentType, u32)> {
        self.labels
            .borrow()
            .get(&name.to_lowercase())
            .map(|x| x.clone())
    }

    fn get_def(&self, name: &String) -> Option<Reg8> {
        self.defs
            .borrow()
            .get(&name.to_lowercase())
            .map(|x| x.clone())
    }

    fn get_set(&self, name: &String) -> Option<Expr> {
        self.sets.borrow().get(name).map(|x| x.clone())
    }

    fn get_special(&self, name: &String) -> Option<Expr> {
        self.special
            .borrow()
            .get(&name.to_lowercase())
            .map(|x| x.clone())
    }

    fn get_device(&self) -> Device {
        self.device
            .borrow()
            .as_ref()
            .unwrap_or(&Device::new(0))
            .clone()
    }

    fn set_define(&self, name: String, expr: Expr) -> Option<Expr> {
        self.defines.borrow_mut().insert(name, expr)
    }

    fn set_equ(&self, name: String, expr: Expr) -> Option<Expr> {
        self.equs.borrow_mut().insert(name.to_lowercase(), expr)
    }

    fn set_label(&self, name: String, value: (SegmentType, u32)) -> Option<(SegmentType, u32)> {
        self.labels.borrow_mut().insert(name, value)
    }

    fn set_def(&self, name: String, value: Reg8) -> Option<Reg8> {
        if self.exist(&name) {
            None
        } else {
            self.defs.borrow_mut().insert(name.to_lowercase(), value)
        }
    }

    fn set_special(&self, name: String, value: Expr) -> Option<Expr> {
        self.special.borrow_mut().insert(name, value)
    }
}
