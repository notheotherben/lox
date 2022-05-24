mod class;
mod env;
mod fun;
mod inter;
mod value;
mod visitor;

pub use class::Class;
pub use fun::{Fun, NativeFun, Closure};
pub use inter::Interpreter;
pub use value::Value;

use crate::{ast::{Stmt, StmtVisitor}, LoxError};

pub fn interpret(stmts: Vec<Stmt>) -> Result<(), LoxError> {
    let mut interpreter = Interpreter::default();
    for stmt in stmts.iter() {
        interpreter.visit_stmt(stmt)?;
    }

    Ok(())
}