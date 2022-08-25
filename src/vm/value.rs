use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::compiler::Primitive;

use super::{Function, Class, Instance};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Primitive(Primitive),
    Pointer(Rc<RefCell<Value>>),
    Function(Rc<Function>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
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
            Value::Primitive(p) => write!(f, "{}", p),
            Value::Pointer(p) => write!(f, "*{}", p.as_ref().borrow()),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Class(c) => write!(f, "{}", c),
            Value::Instance(i) => write!(f, "{}", i.as_ref().borrow()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    stack_offset: Option<usize>,
    pub closed: Option<Rc<RefCell<Value>>>,
}

impl Upvalue {
    pub fn open(stack_offset: usize) -> Self {
        Upvalue {
            stack_offset: Some(stack_offset),
            closed: None,
        }
    }

    pub fn close(&mut self, value: RefCell<Value>) -> Rc<RefCell<Value>> {
        self.stack_offset = None;
        self.closed = Some(Rc::new(value));

        self.closed.clone().unwrap()
    }

    pub fn is_closed(&self) -> bool {
        self.closed.is_some()
    }

    pub fn index(&self) -> Option<usize> {
        self.stack_offset
    }
}

impl PartialEq for Upvalue {
    fn eq(&self, other: &Self) -> bool {
        self.stack_offset == other.stack_offset && self.closed.as_ref() == other.closed.as_ref()
    }
}

impl Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.stack_offset {
            Some(offset) => write!(f, "open upvalue [{}]", offset),
            None => write!(f, "closed upvalue [{:?}]", self.closed.as_ref()),
        }
    }
}
