use std::{rc::Rc, fmt::{Debug, Display}};

use crate::{compiler::{Chunk, OpCode, Primitive, VarRef}, errors, Loc, LoxError};

use super::{gc::Collectible, upvalue::Upvalue, Alloc, Allocator, Function, GC};
use crate::compiler::Function as CFunction;

#[derive(Clone)]
pub struct Frame {
    pub name: Rc<String>,
    pub chunk: Rc<Chunk>,
    pub upvalues: Vec<Alloc<Upvalue>>,
    pub stack_offset: usize,
    pub ip: usize,
    pub fast_call: bool,
}

impl Frame {
    pub fn root_function(func: CFunction, gc: &mut GC) -> Frame {
        let CFunction { name, arity, upvalues, chunk } = func;
        
        if arity != 0 {
            panic!("Root frame must have zero arity.");
        }

        let upvalues = upvalues.iter().map(|upvalue| {
            if let VarRef::Local(idx) = upvalue {
                gc.alloc(Upvalue::Open(*idx))
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
            fast_call: true,
        }
    }

    pub fn root_chunk(chunk: Chunk) -> Frame {
        Frame {
            name: Rc::new(String::default()),
            chunk: Rc::new(chunk),
            upvalues: Vec::new(),
            stack_offset: 0,
            ip: 0,
            fast_call: true,
        }
    }

    pub fn call(fun: Alloc<Function>, stack_size: usize, fast_call: bool) -> Self {
        match fun.as_ref() {
            Function::Native { name, arity, .. } => {
                Frame {
                    name: name.clone(),
                    chunk: Rc::new(Chunk::default()),
                    upvalues: Vec::new(),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                    fast_call,
                }
            }
            Function::Closure { name, arity, upvalues, chunk } => {
                Frame {
                    name: name.clone(),
                    chunk: chunk.clone(),
                    upvalues: upvalues.to_vec(),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                    fast_call,
                }
            }
        }
    }

    pub fn constant(&self, idx: usize) -> Result<&Primitive, LoxError> {
        self.chunk.constants.get(idx).ok_or_else(|| errors::runtime(
            self.last_location(),
            "Attempted to retrieve a constant using an invalid index.",
            "Make sure that your bytecode is using the correct indexes to access constants within each chunk."
        ))
    }

    pub fn upvalue_alloc(&self, idx: usize) -> Result<Alloc<Upvalue>, LoxError> {
        self.upvalues.get(idx).copied().ok_or_else(|| errors::runtime(
            self.last_location(),
            "Attempted to retrieve an upvalue using an invalid index.",
            "Make sure that your bytecode is using the correct indexes to access upvalues within each frame."
        ))
    }

    pub fn upvalue(&self, idx: usize) -> Result<&Upvalue, LoxError> {
        self.upvalues.get(idx).map(|u| u.as_ref()).ok_or_else(|| errors::runtime(
            self.last_location(),
            "Attempted to retrieve an upvalue using an invalid index.",
            "Make sure that your bytecode is using the correct indexes to access upvalues within each frame."
        ))
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

impl Collectible for Frame {
    fn gc(&self) {
        for upvalue in self.upvalues.iter() {
            upvalue.gc();
        }
    }
}