use crate::{lexer::Token};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break,
    Expression(Expr),
    Fun(Token, Vec<Token>, Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Var(Token, Expr),
    While(Expr, Box<Stmt>),
}

pub trait StmtVisitor<T>: ExprVisitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        match stmt {
            Stmt::Block(stmts) => self.visit_block(stmts),
            Stmt::Break => self.visit_break(),
            Stmt::Expression(expr) => self.visit_stmt_expr(expr),
            Stmt::Fun(name, params, body) => self.visit_fun(name, params, body),
            Stmt::If(expr, then_branch, else_branch) => self.visit_if(expr, then_branch, else_branch.as_ref().map(|b| b.as_ref())),
            Stmt::Print(expr) => self.visit_print(expr),
            Stmt::Var(name, expr) => self.visit_var_def(name, expr),
            Stmt::While(expr, body) => self.visit_while(expr, body),
        }
    }

    fn visit_print(&mut self, expr: &Expr) -> T;

    fn visit_break(&mut self) -> T;

    fn visit_stmt_expr(&mut self, expr: &Expr) -> T;

    fn visit_fun(&mut self, name: &Token, params: &[Token], body: &[Stmt]) -> T;

    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> T;

    fn visit_block(&mut self, stmts: &[Stmt]) -> T;

    fn visit_if(&mut self, expr: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> T;

    fn visit_while(&mut self, expr: &Expr, body: &Stmt) -> T;
}