use crate::{lexer::Token};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    Var(Token<'a>, Expr<'a>),
}

pub trait StmtVisitor<T>: ExprVisitor<T> {
    fn visit_stmt(&mut self, stmt: Stmt<'_>) -> T {
        match stmt {
            Stmt::Expression(expr) => self.visit_stmt_expr(expr),
            Stmt::Print(expr) => self.visit_print(expr),
            Stmt::Var(name, expr) => self.visit_var_def(name, expr),
        }
    }

    fn visit_print(&mut self, expr: Expr<'_>) -> T;

    fn visit_stmt_expr(&mut self, expr: Expr<'_>) -> T;

    fn visit_var_def(&mut self, name: Token<'_>, expr: Expr<'_>) -> T;
}