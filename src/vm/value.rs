use std::{fmt::{Display, Debug}, ptr::NonNull};

use crate::compiler::Primitive;

use super::{fun::BoundMethod, Alloc, Class, Collectible, Function, Instance};

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Primitive(PrimitiveReference),
    Pointer(Alloc<Value>),
    Function(Alloc<Function>),
    Class(Alloc<Class>),
    Instance(Alloc<Instance>),
    BoundMethod(Alloc<BoundMethod>)
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
            Value::BoundMethod(b) => b.gc(),
            _ => {}
        }
    }

    fn size(&self) -> usize {
        match self {
            Value::String(s) => std::mem::size_of::<Value>() + std::mem::size_of_val(s),
            _ => std::mem::size_of::<Value>()
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
            (Value::BoundMethod(a), Value::BoundMethod(b)) => a.ptr_eq(b),
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
            Value::Primitive(p) => write!(f, "{}", p.as_ref()),
            Value::Pointer(p) => write!(f, "{}", p.as_ref()),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Class(c) => write!(f, "{}", c),
            Value::Instance(i) => write!(f, "{}", i.as_ref()),
            Value::BoundMethod(b) => write!(f, "{}", b.as_ref())
        }
    }
}


pub struct PrimitiveReference(NonNull<Primitive>);

impl PrimitiveReference {
    pub fn new(primitive: &Primitive) -> Self {
        Self(NonNull::from(primitive))
    }
}

impl Copy for PrimitiveReference {}

impl Clone for PrimitiveReference {
    fn clone(&self) -> Self {
        *self
    }
}

impl AsRef<Primitive> for PrimitiveReference {
    fn as_ref(&self) -> &Primitive {
        unsafe { self.0.as_ref() }
    }
}

impl Debug for PrimitiveReference {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::{size_of, size_of_val};

    #[test]
    fn test_size() {
        // TODO: This should be 16 bits but because we store String directly (instead of interning it) it takes 24 bits
        assert_eq!(size_of::<Value>(), 24);
    }

    #[test]
    fn test_value_size_reporting() {
        let primitive = Primitive::Nil;

        assert_eq!(Value::Nil.size(), size_of::<Value>());
        assert_eq!(Value::Bool(false).size(), size_of::<Value>());
        assert_eq!(Value::Number(0.0).size(), size_of::<Value>());
        assert_eq!(Value::String("test".to_string()).size(), size_of::<Value>() + size_of_val(&"test".to_string()));
        assert_eq!(Value::Primitive(PrimitiveReference::new(&primitive)).size(), size_of::<Value>());
    }
}