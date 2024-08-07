use std::fmt::Display;

use crate::{errors, Loc};

use super::Function;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
}

impl Primitive {
    pub fn as_string(&self) -> Result<&String, errors::LoxError> {
        match self {
            Primitive::String(s) => Ok(s),
            other => Err(errors::runtime(
                Loc::Unknown,
                format!("Expected constant reference to be a string, but got {other} instead."),
                "Make sure that your bytecode is using the correct indexes to access constants within each chunk.",
            )),
        }
    }

    pub fn as_function(&self) -> Result<&Function, errors::LoxError> {
        match self {
            Primitive::Function(f) => Ok(f),
            other => Err(errors::runtime(
                Loc::Unknown,
                format!("Expected constant reference to be a function, but got {other} instead."),
                "Make sure that your bytecode is using the correct indexes to access constants within each chunk.",
            )),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Primitive::Nil => write!(f, "nil"),
            Primitive::Bool(b) => write!(f, "{}", *b),
            Primitive::Number(n) => write!(f, "{}", *n),
            Primitive::String(s) => write!(f, "{}", s),
            Primitive::Function(fun) => write!(f, "{}", fun),
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