use std::{fmt::{Display, Debug}, rc::Rc};

use crate::{LoxError, lexer::Token, ast::{Stmt, StmtVisitor}};

use super::{Value, Interpreter, env::Environment};

#[derive(Clone)]
pub enum Fun {
    Native(Rc<NativeFun>),
    Closure(Closure),
}

impl Fun {
    pub fn native<T: Fn(&mut Interpreter, Vec<Value>) -> Result<Value, LoxError> + 'static, S: Into<String>>(name: S, arity: usize, fun: T) -> Self {
        Fun::Native(Rc::new(NativeFun::new(name, arity, fun)))
    }

    pub fn closure<S: Into<String>>(name: S, params: &[Token], body: &[Stmt], env: Environment) -> Self {
        Fun::Closure(Closure::new(name.into(), params, body, env))
    }

    pub fn name(&self) -> &str {
        match self {
            Fun::Native(fun) => fun.name(),
            Fun::Closure(closure) => closure.name(),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Fun::Native(fun) => fun.arity(),
            Fun::Closure(closure) => closure.arity(),
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, LoxError> {
        match self {
            Fun::Native(fun) => fun.call(interpreter, args),
            Fun::Closure(closure) => closure.call(interpreter, args),
        }
    }

    pub fn bind(&self, this: Value) -> Self {
        match self {
            Fun::Native(fun) => Fun::Native(fun.clone()),
            Fun::Closure(fun) => Fun::Closure(fun.bind(this)),
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

#[derive(Clone)]
pub struct Closure {
    pub name: String,
    pub args: Vec<Token>,
    pub body: Vec<Stmt>,
    pub closure: Environment,
}

impl Closure {
    pub fn new(name: String, args: &[Token], body: &[Stmt], env: Environment) -> Self {
        Self { name, args: args.into(), body: body.into(), closure: env }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, LoxError> {
        let mut env = self.closure.branch();
        self.args.iter().zip(args).for_each(|(arg, value)| {
            env.define(arg.lexeme(), value);
        });

        let old_env = interpreter.env.clone();
        interpreter.env = env;

        let result = interpreter.visit_block(&self.body);

        interpreter.env = old_env;
        
        result?;

        let result = interpreter.returning.clone().unwrap_or(Value::Nil);
        interpreter.returning = None;

        Ok(result)
    }

    pub fn bind(&self, this: Value) -> Self {
        let mut closure = self.closure.branch();
        closure.define("this", this);

        Self::new(self.name.clone(), &self.args, &self.body, closure)
    }
}