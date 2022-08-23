use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::LoxError;

use super::{Chunk, Value, VM, VarRef, value::Upvalue};

#[derive(Clone)]
pub enum Function {
    Native {
        name: Rc<String>,
        arity: usize,
        #[allow(clippy::type_complexity)]
        fun: Rc<Box<dyn Fn(&mut VM) -> Result<Value, LoxError> + 'static>>,
    },
    OpenClosure {
        name: Rc<String>,
        arity: usize,
        upvalues: Vec<VarRef>,
        chunk: Rc<Chunk>,
    },
    ClosedClosure {
        name: Rc<String>,
        arity: usize,
        upvalues: Vec<Upvalue>,
        chunk: Rc<Chunk>,
    },
}

impl Function {
    pub fn closure<S: Into<String>>(name: S, arity: usize, upvalues: Vec<VarRef>, chunk: Chunk) -> Self {
        Function::OpenClosure {
            name: Rc::new(name.into()),
            arity,
            upvalues,
            chunk: Rc::new(chunk),
        }
    }

    pub fn capture<C: FnMut(&Vec<VarRef>) -> Result<Vec<Upvalue>, LoxError>>(&self, mut capture: C) -> Result<Self, LoxError> {
        match self {
            Function::OpenClosure { name, arity, upvalues, chunk } => Ok(Function::ClosedClosure {
                name: name.clone(),
                arity: *arity,
                upvalues: capture(upvalues)?,
                chunk: chunk.clone(),
            }),
            _ => Err(crate::errors::system(
                format!("Attempted to construct a closure from a non-chunk function {}.", self),
                "Please report this error to us on GitHub with example code."))
        }
    }

    pub fn native<N: Into<String>, F: Fn(&mut VM) -> Result<Value, LoxError> + 'static>(
        name: N,
        arity: usize,
        fun: F,
    ) -> Self {
        Function::Native {
            name: Rc::new(name.into()),
            arity,
            fun: Rc::new(Box::new(fun)),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Function::OpenClosure {
                    name: name1,
                    arity: arity1,
                    chunk: chunk1,
                    ..
                },
                Function::OpenClosure {
                    name: name2,
                    arity: arity2,
                    chunk: chunk2,
                    ..
                },
            ) => {
                name1 == name2
                    && arity1 == arity2
                    && Rc::ptr_eq(chunk1, chunk2)
            },
            (
                Function::ClosedClosure { name, arity, upvalues, chunk },
                Function::ClosedClosure { name: name2, arity: arity2, upvalues: upvalues2, chunk: chunk2 },
            ) => {
                name == name2 && arity == arity2 && upvalues == upvalues2 && Rc::ptr_eq(chunk, chunk2)
            },
            _ => false,
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
        match self {
            Function::OpenClosure { name, .. } => write!(f, "<chunk {}>", name),
            Function::Native { name, .. } => write!(f, "<native {}>", name),
            Function::ClosedClosure { name, .. } => write!(f, "<fn {}>", name),
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::OpenClosure { chunk, .. } => write!(f, "<chunk>\n{}", chunk),
            Function::Native { name, .. } => write!(f, "<native {}>", name),
            Function::ClosedClosure { name, chunk, .. } => write!(f, "<fn {}>\n{}", name, chunk),
        }
    }
}
