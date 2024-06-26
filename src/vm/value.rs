use std::{fmt::Display, rc::Rc};

use crate::compiler::Primitive;

use super::{gc::{Allocator, GC}, Alloc, Class, Collectible, Function, Instance};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Primitive(Primitive),
    Pointer(Alloc<Value>),
    Function(Rc<Function>),
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

impl Collectible for Value {
    fn mark(&self, gc: &mut GC) {
        match self {
            Value::Pointer(p) => gc.mark(*p),
            Value::Function(f) => f.mark(gc),
            Value::Class(c) => c.mark(gc),
            Value::Instance(i) => i.mark(gc),
            _ => {}
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
            Value::Pointer(p) => write!(f, "*{}", p),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Class(c) => write!(f, "{}", c),
            Value::Instance(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Upvalue {
    Open(usize),
    Closed(Alloc<Value>),
}

impl Alloc<Upvalue> {
    pub fn close(&mut self, value: Alloc<Value>) {
        self.replace_value(Upvalue::Closed(value));
    }
}

impl Collectible for Upvalue {
    fn mark(&self, gc: &mut GC) {
        if let Upvalue::Closed(closed) = self { 
            gc.mark(*closed);
        }
    }
}

impl Display for Upvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Upvalue::Open(index) => write!(f, "open upvalue [{}]", *index),
            Upvalue::Closed(value) => write!(f, "closed upvalue [{}]", value),
        }
    }
}
