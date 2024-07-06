use crate::{LoxError, ast::{Stmt, StmtVisitor}};

mod chunk;
mod comp;
mod fun;
mod ops;
mod oprunner;
mod value;

pub use chunk::Chunk;
pub use fun::Function;
pub use ops::OpCode;
pub use oprunner::OpRunner;
pub use value::{Primitive, VarRef};

pub fn compile(stmts: &[Stmt]) -> Result<Function, LoxError> {
    let mut compiler =  comp::Compiler::new();

    for stmt in stmts {
        compiler.visit_stmt(stmt)?;
    }

    Ok(compiler.finalize())
}