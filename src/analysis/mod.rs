use crate::{LoxError, ast::{Stmt, StmtVisitor}};

mod vars;

pub fn analyze(stmts: &[Stmt]) -> Vec<LoxError> {
    let mut errs = Vec::new();
    let mut analyzers = vec![
        // Static analysis which ensures that variables are defined before they are used.
        vars::VariableAnalyzer::default()
    ];

    for stmt in stmts {
        for analyzer in analyzers.iter_mut() {
            errs.append(&mut analyzer.visit_stmt(stmt));
        }
    }

    errs
}