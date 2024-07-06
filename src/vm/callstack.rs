use std::fmt::Debug;

use crate::{errors, LoxError};

use super::{frame::Frame, gc::Collectible};

const MAX_CALLSTACK_DEPTH: usize = 1000;

pub struct CallStack(Vec<Frame>);

impl CallStack {
    pub fn stacktrace(&self) -> Vec<String> {
        self.0
            .iter()
            .rev()
            .map(|f| format!("{}", f))
            .collect()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn pop(&mut self) -> Option<Frame> {
        self.0.pop()
    }

    pub fn push(&mut self, frame: Frame) -> Result<(), LoxError> {
        if self.0.len() >= MAX_CALLSTACK_DEPTH {
            Err(errors::runtime_stacktrace(
                "The maximum call stack size has been exceeded.",
                "Make sure that you are not calling functions with unbounded recursion.",
                self.stacktrace(),
            ))
        } else {
            self.0.push(frame);
            Ok(())
        }
    }

    pub fn active(&self) -> Result<&Frame, LoxError> {
        self.0.last().ok_or_else(|| errors::runtime(
            crate::Loc::Native,
            "Attempted to access the active frame of an empty call stack.",
            "Make sure that you are not attempting to access the callstack frame outside of the VM run loop.",
        ))
    }

    pub fn active_mut(&mut self) -> Result<&mut Frame, LoxError> {
        self.0.last_mut().ok_or_else(|| errors::runtime(
            crate::Loc::Native,
            "Attempted to access the active frame of an empty call stack.",
            "Make sure that you are not attempting to access the callstack frame outside of the VM run loop.",
        ))
    }
}

impl Default for CallStack {
    fn default() -> Self {
        CallStack(Vec::with_capacity(MAX_CALLSTACK_DEPTH))
    }
}

impl Collectible for CallStack {
    fn gc(&self) {
        for frame in self.0.iter() {
            frame.gc();
        }
    }
}

impl Debug for CallStack {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for frame in self.0.iter() {
            write!(f, "{}", frame)?;
        }

        Ok(())
    }
}