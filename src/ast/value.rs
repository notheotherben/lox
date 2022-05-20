use std::{cmp::Ordering, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    False,
    True,
    Number(f64),
    String(String)
}

impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => a.partial_cmp(b),
            (Literal::String(a), Literal::String(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::False => write!(f, "false"),
            Literal::True => write!(f, "true"),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s)
        }
    }
}