use std::{rc::Rc, fmt::{Debug, Display}};

use crate::Loc;

use super::{Chunk, OpCode, Value};

#[derive(Clone)]
pub struct Frame {
    pub name: Rc<String>,
    pub chunk: Rc<Chunk>,
    pub stack_offset: usize,
    pub ip: usize,
}

impl Frame {
    pub fn root(chunk: Chunk) -> Frame {
        Frame {
            name: Rc::new(String::default()),
            chunk: Rc::new(chunk),
            stack_offset: 0,
            ip: 0,
        }
    }

    pub fn call(name: Rc<String>, chunk: Rc<Chunk>, stack_offset: usize) -> Self {
        Frame {
            name,
            chunk,
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
        self.chunk.location(self.ip - 1)
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