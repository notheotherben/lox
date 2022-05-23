use std::{rc::Rc, sync::RwLock};

use crate::{ast::{StmtVisitor, Stmt}, LoxError, errors};

use super::{env::Environment, Value, Fun};

#[derive(Debug, Clone)]
pub struct Interpreter{
    pub (crate) env: Rc<RwLock<Environment>>,
    pub (crate) breaking: bool,
    pub (crate) returning: Option<Value>,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), LoxError> {
        for stmt in stmts.iter() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut globals = Environment::default();
        globals.define("clock", Value::Callable(Fun::native("native@clock", 0, |_, _| {
            let offset = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH)
                .map_err(|e| errors::system_with_internal(
                    "Failed to get current system time because the system time is currently set to a time earlier than 1970-01-01T00:00:00Z.",
                    "Make sure that you have set your system clock correctly.",
                    e))?;
                
            Ok(Value::Number(offset.as_secs() as f64))
        })));

        globals.define("assert", Value::Callable(Fun::native("native@assert", 2, |_, args| {
            if !args[0].is_truthy() {
                return Err(errors::user_with_internal(
                    "Assertion failed",
                    "Make sure that the first argument passed to the assert function is truthy.",
                    human_errors::detailed_message(&args[1].to_string()),
                ));
            }

            Ok(Value::Nil)
        })));

        Self {
            env: Environment::child(Rc::new(RwLock::new(globals))),
            breaking: false,
            returning: None,
        }
    }
}