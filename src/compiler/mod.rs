use crate::{vm::Function, LoxError, ast::{Stmt, StmtVisitor}};

mod comp;

pub fn compile(stmts: &[Stmt]) -> Result<Function, LoxError> {
    let mut compiler =  comp::Compiler::new();

    for stmt in stmts {
        compiler.visit_stmt(stmt)?;
    }

    Ok(compiler.finalize())
}