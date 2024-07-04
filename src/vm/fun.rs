use std::{
    fmt::{Debug, Display}, mem::{size_of, size_of_val}, rc::Rc
};

use crate::{LoxError, compiler::{Function as CFunction, VarRef, Chunk}};

use super::{class::Instance, upvalue::Upvalue, Alloc, Collectible, Value, VM};

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
        upvalues: Vec<Alloc<Upvalue>>,
        chunk: Rc<Chunk>,
    },
}

impl Function {
    pub fn capture<C: FnMut(&[VarRef]) -> Result<Vec<Alloc<Upvalue>>, LoxError>>(fun: &CFunction, mut capture: C) -> Result<Self, LoxError> {
        Ok(Function::Closure {
            name: fun.name.clone(),
            arity: fun.arity,
            upvalues: capture(&fun.upvalues)?,
            chunk: fun.chunk.clone(),
        })
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

    pub fn arity(&self) -> usize {
        match self {
            Function::Native { arity, .. } => *arity,
            Function::Closure { arity, .. } => *arity,
        }
    }
}

impl Collectible for Function {
    fn gc(&self) {
        match self {
            Function::Native { .. } => {},
            Function::Closure { upvalues, .. } => {
                for upvalue in upvalues.iter() {
                    upvalue.gc();
                }
            },
        }
    }

    fn size(&self) -> usize {
        match self {
            Function::Native { .. } => size_of::<Self>(),
            Function::Closure { name, upvalues, .. } => size_of::<Self>() + size_of_val(name) + size_of_val(upvalues),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Function::Native { name: name1, arity: arity1, .. }, Function::Native { name: name2, arity: arity2, .. }) => {
                name1 == name2 && arity1 == arity2
            }
            (
                Function::Closure { name, arity, upvalues, chunk },
                Function::Closure { name: name2, arity: arity2, upvalues: upvalues2, chunk: chunk2 },
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
            Function::Native { name, arity, .. } => write!(f, "fun native@{name} ({arity} args)"),
            Function::Closure { name, arity, .. } => write!(f, "fun {name} ({arity} args)"),
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::Native { name, .. } => write!(f, "<native {}>", name),
            Function::Closure { name, chunk, .. } => write!(f, "<fn {}>\n{}", name, chunk),
        }
    }
}

pub struct BoundMethod(pub Alloc<Instance>, pub Alloc<Function>);

impl Collectible for BoundMethod {
    fn gc(&self) {
        self.0.gc();
        self.1.gc();
    }
}

impl Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl Debug for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.1)
    }
}