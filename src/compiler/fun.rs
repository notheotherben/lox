use std::{fmt::{Display, Debug}, rc::Rc};

use super::{VarRef, Chunk};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub arity: usize,
    pub upvalues: Vec<VarRef>,
    pub chunk: Rc<Chunk>,
}

impl Function {
    pub fn new<S: Into<String>>(name: S, arity: usize, upvalues: Vec<VarRef>, chunk: Chunk) -> Self {
        Function {
            name: Rc::new(name.into()),
            arity,
            upvalues,
            chunk: Rc::new(chunk),
        }
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<fn {}>\n{}", self.name, &self.chunk)
    }
}