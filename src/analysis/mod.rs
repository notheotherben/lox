use crate::{LoxError, ast::{Stmt, StmtVisitor}};

mod vars;

pub trait Analyzer: StmtVisitor<Vec<LoxError>> {
    fn analyze(&mut self, stmts: &[Stmt]) -> Vec<LoxError> {
        let mut errors = Vec::new();
        for stmt in stmts {
            errors.extend(self.visit_stmt(stmt));
        }
        errors
    }
}

pub fn analyze(stmts: &[Stmt]) -> Vec<LoxError> {
    vec![
        // Static analysis which ensures that variables are defined before they are used.
        vars::VariableAnalyzer::default().analyze(stmts)
    ].into_iter().flatten().collect()
}