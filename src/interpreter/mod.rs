mod visitor;

pub use visitor::Interpreter;

use crate::{ast::{Stmt, StmtVisitor}, LoxError};

pub fn interpret(stmts: Vec<Stmt<'_>>) -> Result<(), LoxError> {
    let interpreter = Interpreter{};
    for stmt in stmts {
        interpreter.visit_stmt(stmt)?;
    }

    Ok(())
}