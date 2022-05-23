use std::{fmt::{Display, Debug}, rc::Rc};

use crate::{LoxError};

use super::{Value, Interpreter};

#[derive(Clone)]
pub enum Fun {
    Native(Rc<NativeFun>),
}

impl Fun {
    pub fn native<T: Fn(&mut Interpreter, Vec<Value>) -> Result<Value, LoxError> + 'static, S: Into<String>>(name: S, arity: usize, fun: T) -> Self {
        Fun::Native(Rc::new(NativeFun::new(name, arity, fun)))
    }

    pub fn name(&self) -> &str {
        match self {
            Fun::Native(fun) => fun.name(),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Fun::Native(fun) => fun.arity(),
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, LoxError> {
        match self {
            Fun::Native(fun) => fun.call(interpreter, args),
        }
    }
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "fun {} ({} args)", self.name(), self.arity())
    }
}

impl Debug for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl PartialEq for Fun {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

pub struct NativeFun {
    pub name: String,
    pub arity: usize,
    #[allow(clippy::type_complexity)]
    pub fun: Box<dyn Fn(&mut Interpreter, Vec<Value>) -> Result<Value, LoxError>>,
}

impl NativeFun {
    pub fn new<T: Fn(&mut Interpreter, Vec<Value>) -> Result<Value, LoxError> + 'static, S: Into<String>>(name: S, arity: usize, fun: T) -> Self {
        Self { name: name.into(), arity, fun: Box::new(fun) }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, LoxError> {
        (self.fun)(interpreter, args)
    }
}