use crate::{lexer::Token};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Break,
    Print(Expr<'a>),
    Var(Token<'a>, Expr<'a>),
    Block(Vec<Stmt<'a>>),
    If(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    While(Expr<'a>, Box<Stmt<'a>>),
}

pub trait StmtVisitor<T>: ExprVisitor<T> {
    fn visit_stmt(&mut self, stmt: Stmt<'_>) -> T {
        match stmt {
            Stmt::Expression(expr) => self.visit_stmt_expr(expr),
            Stmt::Break => self.visit_break(),
            Stmt::Print(expr) => self.visit_print(expr),
            Stmt::Var(name, expr) => self.visit_var_def(name, expr),
            Stmt::Block(stmts) => self.visit_block(stmts),
            Stmt::If(expr, then_branch, else_branch) => self.visit_if(expr, *then_branch, else_branch.map(|b| *b)),
            Stmt::While(expr, body) => self.visit_while(expr, *body),
        }
    }

    fn visit_print(&mut self, expr: Expr<'_>) -> T;

    fn visit_break(&mut self) -> T;

    fn visit_stmt_expr(&mut self, expr: Expr<'_>) -> T;

    fn visit_var_def(&mut self, name: Token<'_>, expr: Expr<'_>) -> T;

    fn visit_block(&mut self, stmts: Vec<Stmt<'_>>) -> T;

    fn visit_if(&mut self, expr: Expr<'_>, then_branch: Stmt<'_>, else_branch: Option<Stmt<'_>>) -> T;

    fn visit_while(&mut self, expr: Expr<'_>, body: Stmt<'_>) -> T;
}