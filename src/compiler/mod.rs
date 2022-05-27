use crate::{vm::Chunk, LoxError, ast::{Stmt, StmtVisitor}};

mod comp;

pub fn compile(stmts: &[Stmt]) -> Result<Chunk, LoxError> {
    let mut compiler = comp::Compiler::default();

    for stmt in stmts {
        compiler.visit_stmt(stmt)?;
    }

    Ok(compiler.chunk)
}