mod env;
mod fun;
mod inter;
mod value;
mod visitor;

pub use fun::{Fun, NativeFun};
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