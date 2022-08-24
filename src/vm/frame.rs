use std::{rc::Rc, fmt::{Debug, Display}};

use crate::Loc;

use super::{Chunk, OpCode, Value, value::Upvalue, Function, VarRef};

#[derive(Clone)]
pub struct Frame {
    pub name: Rc<String>,
    pub chunk: Rc<Chunk>,
    pub upvalues: Vec<Rc<Upvalue>>,
    pub stack_offset: usize,
    pub ip: usize,
}

impl Frame {
    pub fn root_function(func: Function) -> Frame {
        if let Function::OpenClosure { name, arity, upvalues, chunk } = func {
            if arity != 0 {
                panic!("Root frame must have zero arity.");
            }

            let upvalues = upvalues.iter().map(|upvalue| {
                if let VarRef::Local(idx) = upvalue {
                    Rc::new(Upvalue::Open(*idx))
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
        } else {
            panic!("Attempted to construct a root frame using an unsupported function type {}.", func);
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

    pub fn call(name: Rc<String>, upvalues: Vec<Rc<Upvalue>>, chunk: Rc<Chunk>, stack_offset: usize) -> Self {
        Frame {
            name,
            chunk,
            upvalues,
            stack_offset,
            ip: 0,
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn constant(&self, idx: usize) -> Option<&Value> {
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