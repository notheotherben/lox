use std::{rc::Rc, fmt::{Debug, Display}};

use crate::{Loc, compiler::{Chunk, VarRef, Primitive, OpCode}};

use super::{gc::Collectible, value::Upvalue, Alloc, Allocator, Function, GC};
use crate::compiler::Function as CFunction;

#[derive(Clone)]
pub struct Frame {
    pub name: Rc<String>,
    pub chunk: Rc<Chunk>,
    pub upvalues: Rc<Vec<Alloc<Upvalue>>>,
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
            upvalues: Rc::new(upvalues),
            stack_offset: 0,
            ip: 0,
            fast_call: true,
        }
    }

    pub fn root_chunk(chunk: Chunk) -> Frame {
        Frame {
            name: Rc::new(String::default()),
            chunk: Rc::new(chunk),
            upvalues: Rc::new(Vec::new()),
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
                    upvalues: Rc::new(Vec::new()),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                    fast_call,
                }
            }
            Function::Closure { name, arity, upvalues, chunk } => {
                Frame {
                    name: name.clone(),
                    chunk: chunk.clone(),
                    upvalues: Rc::new(upvalues.to_vec()),
                    stack_offset: stack_size - *arity - 1,
                    ip: 0,
                    fast_call,
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

impl Collectible for Frame {
    fn gc(&self) {
        for upvalue in self.upvalues.iter() {
            upvalue.gc();
        }
    }
}