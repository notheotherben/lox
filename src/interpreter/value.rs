use std::cmp::Ordering;

use crate::ast::Literal;

use super::Fun;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Callable(Fun)
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(fun) => write!(f, "fun ({} args)", fun.arity()),
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        }
    }
}
