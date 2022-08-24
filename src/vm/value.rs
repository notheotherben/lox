use std::{fmt::Display, rc::Rc};

use super::{Function, Class, Instance};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    Class(Rc<Class>),
    Instance(Instance),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", *b),
            Value::Number(n) => write!(f, "{}", *n),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Class(cls) => write!(f, "{}", cls),
            Value::Instance(inst) => write!(f, "{}", inst),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Rc<Value>),
}

impl PartialEq for Upvalue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Upvalue::Open(idx1), Upvalue::Open(idx2)) => idx1 == idx2,
            (Upvalue::Closed(val1), Upvalue::Closed(val2)) => Rc::ptr_eq(val1, val2),
            _ => false
        }
    }
}

impl Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Upvalue::Open(idx) => write!(f, "open upvalue {}", *idx),
            Upvalue::Closed(value) => write!(f, "closed upvalue [{}]", value)
        }
    }
}

#[derive(Debug, Clone)]
pub enum VarRef {
    Local(usize),
    Transitive(usize),
}

impl PartialEq for VarRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VarRef::Local(idx1), VarRef::Local(idx2)) => idx1 == idx2,
            (VarRef::Transitive(idx1), VarRef::Transitive(idx2)) => idx1 == idx2,
            _ => false,
        }
    }
}

impl Display for VarRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarRef::Local(_) => write!(f, "local"),
            VarRef::Transitive(_) => write!(f, "upvalue"),
        }
    }
}