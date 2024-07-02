use std::{fmt::Display, mem::{size_of, size_of_val}};

use crate::compiler::Primitive;

use super::{Alloc, Class, Collectible, Function, Instance};

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Primitive(Primitive),
    Pointer(Alloc<Value>),
    Function(Alloc<Function>),
    Class(Alloc<Class>),
    Instance(Alloc<Instance>),
    BoundMethod(Alloc<Instance>, Alloc<Function>)
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
    fn gc(&self) {
        match self {
            Value::Pointer(p) => p.gc(),
            Value::Function(f) => f.gc(),
            Value::Class(c) => c.gc(),
            Value::Instance(i) => i.gc(),
            Value::BoundMethod(i, f) => {
                i.gc();
                f.gc();
            },
            _ => {}
        }
    }

    fn size(&self) -> usize {
        match self {
            Value::Pointer(p) => size_of::<Self>() + p.size(),
            Value::Function(f) => size_of::<Self>() + f.size(),
            Value::Class(c) => size_of::<Self>() + c.size(),
            Value::Instance(i) => size_of::<Self>() + i.size(),
            Value::String(s) => size_of::<Self>() + size_of_val(s),
            Value::BoundMethod(i, f) => size_of::<Self>() + i.size() + f.size(),
            _ => size_of::<Self>(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Class(a), Value::Class(b)) => a.ptr_eq(b),
            (Value::Instance(a), Value::Instance(b)) => a.ptr_eq(b),
            (Value::Function(a), Value::Function(b)) => a.ptr_eq(b),
            (Value::BoundMethod(ai, af), Value::BoundMethod(bi, bf)) => ai.ptr_eq(bi) && af.ptr_eq(bf),
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None
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
            Value::Pointer(p) => write!(f, "{}", p.as_ref()),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Class(c) => write!(f, "{}", c),
            Value::Instance(i) => write!(f, "{}", i.as_ref()),
            Value::BoundMethod(_i, fun) => write!(f, "{}", fun.as_ref())
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
    fn gc(&self) {
        if let Upvalue::Closed(closed) = self { 
            closed.gc();
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn test_value_sizes() {
        assert_eq!(Value::Nil.size(), size_of::<Value>());
        assert_eq!(Value::Bool(false).size(), size_of::<Value>());
        assert_eq!(Value::Number(0.0).size(), size_of::<Value>());
        assert_eq!(Value::String("test".to_string()).size(), size_of::<Value>() + size_of_val(&"test".to_string()));
        assert_eq!(Value::Primitive(Primitive::Nil).size(), size_of::<Value>());
    }
}