use std::fmt::Display;

use super::{Alloc, Collectible, Value};


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