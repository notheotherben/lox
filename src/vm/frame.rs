use std::{rc::Rc, fmt::Debug};

use crate::Loc;

use super::{Chunk, OpCode, Value};

#[derive(Clone)]
pub struct Frame {
    pub chunk: Rc<Chunk>,
    pub stack_offset: usize,
    pub ip: usize,
}

impl Frame {
    pub fn new(chunk: Chunk) -> Frame {
        Frame {
            chunk: Rc::new(chunk),
            stack_offset: 0,
            ip: 0,
        }
    }

    pub fn call(chunk: Rc<Chunk>, stack_offset: usize) -> Self {
        Frame {
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

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.chunk.disassemble(self.ip, f)
    }
}