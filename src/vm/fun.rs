use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::LoxError;

use super::{Chunk, Value, VM};

#[derive(Clone)]
pub enum Function {
    Native {
        name: Rc<String>,
        arity: usize,
        #[allow(clippy::type_complexity)]
        fun: Rc<Box<dyn Fn(&mut VM) -> Result<Value, LoxError> + 'static>>,
    },
    Closure {
        name: Rc<String>,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalues: Vec<Rc<Value>>,
    },
}

impl Function {
    pub fn closure<S: Into<String>>(name: S, arity: usize, chunk: Chunk) -> Self {
        Function::Closure {
            name: Rc::new(name.into()),
            arity,
            chunk: Rc::new(chunk),
            upvalues: Vec::new(),
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
                Function::Closure {
                    name: name1,
                    arity: arity1,
                    chunk: chunk1,
                    upvalues: upvalues1,
                },
                Function::Closure {
                    name: name2,
                    arity: arity2,
                    chunk: chunk2,
                    upvalues: upvalues2,
                },
            ) => {
                name1 == name2
                    && arity1 == arity2
                    && Rc::ptr_eq(chunk1, chunk2)
                    && upvalues1.len() == upvalues2.len()
                    && upvalues1
                        .iter()
                        .zip(upvalues2.iter())
                        .all(|(a, b)| Rc::ptr_eq(a, b))
            }
            _ => false,
        }
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::Closure { name, .. } => write!(f, "<fn {}>", name),
            Function::Native { name, .. } => write!(f, "<native {}>", name),
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::Closure { name, .. } => write!(f, "<fn {}>", name),
            Function::Native { name, .. } => write!(f, "<native {}>", name),
        }
    }
}
