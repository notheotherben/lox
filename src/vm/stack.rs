use crate::{errors, Loc, LoxError};

use super::{gc::Collectible, value::Value};

const STACK_DEPTH_LIMIT: usize = 1000;

pub struct Stack {
    depth: usize,
    stack: [Value;STACK_DEPTH_LIMIT]
}

impl Stack {
    pub fn get(&self, idx: usize) -> Result<Value, LoxError> {
        if idx >= self.depth {
            Err(errors::runtime(
                Loc::Unknown,
                "Attempted to access a value at an index that is out of bounds.",
                "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
            ))
        } else {
            Ok(self.stack[idx])
        }
    }

    pub fn get_from_top(&self, offset: usize) -> Result<Value, LoxError> {
        let idx = self.depth.checked_sub(offset).ok_or_else(|| errors::runtime(
            Loc::Unknown,
            "Attempted to access a value at an index that is out of bounds.",
            "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
        ))?;

        self.get(idx)
    }

    pub fn slice_from_top(&self, offset: usize) -> Result<&[Value], LoxError> {
        let idx = self.depth.checked_sub(offset).ok_or_else(|| errors::runtime(
            Loc::Unknown,
            "Attempted to access a value at an index that is out of bounds.",
            "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
        ))?;

        Ok(&self.stack[idx..self.depth])
    }

    pub fn replace(&mut self, idx: usize, value: Value) -> Result<(), LoxError> {
        if idx >= self.depth {
            Err(errors::runtime(
                Loc::Unknown,
                "Attempted to replace a value at an index that is out of bounds.",
                "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
            ))
        } else {
            self.stack[idx] = value;
            Ok(())
        }
    }

    pub fn replace_from_top(&mut self, offset: usize, value: Value) -> Result<(), LoxError> {
        let idx = self.depth.checked_sub(offset).ok_or_else(|| errors::runtime(
            Loc::Unknown,
            "Attempted to replace a value at an index that is out of bounds.",
            "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
        ))?;

        self.replace(idx, value)
    }

    pub fn push(&mut self, value: Value) -> Result<(), LoxError> {
        if self.depth == STACK_DEPTH_LIMIT {
            Err(errors::runtime(
                Loc::Unknown,
                "The maximum stack size has been exceeded.",
                "Make sure that you are not calling functions with unbounded recursion.",
            ))
        } else {
            self.stack[self.depth] = value;
            self.depth += 1;
            Ok(())
        }
    }

    pub fn peek(&self) -> Result<Value, LoxError> {
        if self.depth == 0 {
            Err(errors::runtime(
                Loc::Unknown,
                "Attempted to peek at the top of an empty stack.",
                "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
            ))
        } else {
            Ok(self.stack[self.depth - 1])
        }
    }

    pub fn pop(&mut self) -> Result<Value, LoxError> {
        if self.depth == 0 {
            Err(errors::runtime(
                Loc::Unknown,
                "Attempted to pop a value from an empty stack.",
                "Make sure that your bytecode is not issuing an invalid instruction given the state of the system.",
            ))
        } else {
            let value = self.stack[self.depth - 1];
            self.stack[self.depth - 1] = Value::Nil;
            self.depth -= 1;
            Ok(value)
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len >= self.depth {
            return;
        }

        for i in len..self.depth {
            self.stack[i] = Value::Nil;
        }
        self.depth = len;
    }

    pub fn is_empty(&self) -> bool {
        self.depth == 0
    }

    pub fn len(&self) -> usize {
        self.depth
    }
}

impl Default for Stack {
    fn default() -> Self {
        Stack {
            depth: 0,
            stack: [Value::Nil;STACK_DEPTH_LIMIT]
        }
    }

}

impl Collectible for Stack {
    fn gc(&self) {
        for value in self.stack[..self.depth].iter() {
            value.gc();
        }
    }
}

impl std::fmt::Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for value in self.stack[..self.depth].iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;
        Ok(())
    }
}