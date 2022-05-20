mod env;
mod visitor;

pub use visitor::Interpreter;

use crate::{ast::{Stmt, StmtVisitor}, LoxError};

pub fn interpret(stmts: Vec<Stmt<'_>>) -> Result<(), LoxError> {
    let mut interpreter = Interpreter::default();
    for stmt in stmts {
        interpreter.visit_stmt(stmt)?;
    }

    Ok(())
}