use crate::{lexer::Token, Loc};

use super::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunType {
    Closure,
    Method,
    Initializer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(Loc),
    Class(Token, Option<Expr>, Vec<Stmt>, Vec<Stmt>),
    Expression(Expr),
    Fun(FunType, Token, Vec<Token>, Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Loc, Expr),
    Return(Token, Option<Expr>),
    Var(Token, Expr),
    While(Expr, Box<Stmt>),
}

pub trait StmtVisitor<T>: ExprVisitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        match stmt {
            Stmt::Block(stmts) => self.visit_block(stmts),
            Stmt::Break(loc) => self.visit_break(loc),
            Stmt::Class(name, superclass, statics, methods) => self.visit_class(name, superclass.as_ref(), statics, methods),
            Stmt::Expression(expr) => self.visit_expr_stmt(expr),
            Stmt::Fun(ty, name, params, body) => self.visit_fun_def(*ty, name, params, body),
            Stmt::If(expr, then_branch, else_branch) => self.visit_if(expr, then_branch, else_branch.as_ref().map(|b| b.as_ref())),
            Stmt::Print(loc, expr) => self.visit_print(loc, expr),
            Stmt::Return(name, expr) => self.visit_return(name, expr.as_ref()),
            Stmt::Var(name, expr) => self.visit_var_def(name, expr),
            Stmt::While(expr, body) => self.visit_while(expr, body),
        }
    }

    
    fn visit_break(&mut self, loc: &Loc) -> T;
    
    fn visit_block(&mut self, stmts: &[Stmt]) -> T;
    
    fn visit_class(&mut self, name: &Token, superclass: Option<&Expr>, statics: &[Stmt], methods: &[Stmt]) -> T;

    fn visit_expr_stmt(&mut self, expr: &Expr) -> T;
    
    fn visit_fun_def(&mut self, ty: FunType, name: &Token, params: &[Token], body: &[Stmt]) -> T;

    fn visit_if(&mut self, expr: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> T;
    
    fn visit_print(&mut self, loc: &Loc, expr: &Expr) -> T;

    fn visit_return(&mut self, token: &Token, expr: Option<&Expr>) -> T;
    
    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> T;

    fn visit_while(&mut self, expr: &Expr, body: &Stmt) -> T;
}