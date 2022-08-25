use std::{rc::Rc, fmt::{Debug, Display}, cell::RefCell};

use crate::{Loc, compiler::{Chunk, VarRef, Primitive, OpCode}};

use super::{value::Upvalue, Function};
use crate::compiler::Function as CFunction;

#[derive(Clone)]
pub struct Frame {
    pub name: Rc<String>,
    pub chunk: Rc<Chunk>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
    pub stack_offset: usize,
    pub ip: usize,
}

impl Frame {
    pub fn root_function(func: CFunction) -> Frame {
        let CFunction { name, arity, upvalues, chunk } = func;
        
        if arity != 0 {
            panic!("Root frame must have zero arity.");
        }

        let upvalues = upvalues.iter().map(|upvalue| {
            if let VarRef::Local(idx) = upvalue {
                Rc::new(RefCell::new(Upvalue::open(*idx)))
            } else {
                panic!("Attempted to construct a root frame with a non-local upvalue.")
            }
        }).collect();

        Frame {
            name,
            chunk,
            upvalues,
            stack_offset: 0,
            ip: 0,
        }
    }

    pub fn root_chunk(chunk: Chunk) -> Frame {
        Frame {
            name: Rc::new(String::default()),
            chunk: Rc::new(chunk),
            upvalues: Vec::new(),
            stack_offset: 0,
            ip: 0,
        }
    }

    pub fn call(fun: Rc<Function>, stack_size: usize) -> Self {
        match fun.as_ref() {
            Function::Native { name, arity, .. } => {
                Frame {
                    name: name.clone(),
                    chunk: Rc::new(Chunk::default()),
                    upvalues: Vec::new(),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                }
            }
            Function::Closure { name, arity, upvalues, chunk } => {
                Frame {
                    name: name.clone(),
                    chunk: chunk.clone(),
                    upvalues: upvalues.clone(),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                }
            }
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn constant(&self, idx: usize) -> Option<&Primitive> {
        self.chunk.constants.get(idx)
    }

    pub fn opcode(&self) -> Option<&OpCode> {
        self.chunk.code.get(self.ip)
    }

    pub fn last_location(&self) -> Loc {
        let ip = if self.ip > 0 { self.ip - 1 } else { 0 };
        self.chunk.location(ip)
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.name.is_empty() {
            write!(f, "[{}] in script", self.chunk.location(self.ip))
        } else {
            write!(f, "[{}] in {}()", self.chunk.location(self.ip), self.name)
        }
    }
}

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.chunk.disassemble(self.ip, f)
    }
}