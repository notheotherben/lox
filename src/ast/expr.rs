use crate::{lexer::Token, Loc};

use super::{Literal, Stmt};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, Token),
    Get(Box<Expr>, Token),
    Fun(Loc, Vec<Token>, Vec<Stmt>),
    Grouping(Box<Expr>),
    Literal(Loc, Literal),
    Logical(Box<Expr>, Token, Box<Expr>),
    Set(Box<Expr>, Token, Box<Expr>),
    Super(Loc, Token),
    This(Loc),
    Unary(Token, Box<Expr>),
    Var(Token),
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T {
        match expr {
            Expr::Assign(ident, value) => {
                self.visit_assign(ident, value)
            },
            Expr::Binary(left, op, right) => {
                self.visit_binary(left, op, right)
            },
            Expr::Call(callee, args, close) => {
                self.visit_call(callee, args, close)
            },
            Expr::Get(obj, name) => {
                self.visit_get(obj, name)
            },
            Expr::Fun(loc, params, body) => {
                self.visit_fun_expr(loc, params, body)
            },
            Expr::Grouping(expr) => {
                self.visit_grouping(expr)
            },
            Expr::Logical(left, op, right) => {
                self.visit_logical(left, op, right)
            },
            Expr::Literal(loc, value) => {
                self.visit_literal(loc, value)
            },
            Expr::Set(obj, property, value) => {
                self.visit_set(obj, property, value)
            },
            Expr::Super(loc, method) => {
                self.visit_super(loc, method)
            },
            Expr::This(loc) => {
                self.visit_this(loc)
            },
            Expr::Unary(op, expr) => {
                self.visit_unary(op, expr)
            },
            Expr::Var(name) => {
                self.visit_var_ref(name)
            }
        }
    }

    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> T;

    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> T;

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], close: &Token) -> T;

    fn visit_get(&mut self, obj: &Expr, property: &Token) -> T;

    fn visit_fun_expr(&mut self, loc: &Loc, params: &[Token], body: &[Stmt]) -> T;

    fn visit_grouping(&mut self, expr: &Expr) -> T;

    fn visit_literal(&mut self, loc: &Loc, value: &Literal) -> T;

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> T;

    fn visit_set(&mut self, obj: &Expr, property: &Token, value: &Expr) -> T;

    fn visit_super(&mut self, loc: &Loc, method: &Token) -> T;

    fn visit_this(&mut self, loc: &Loc) -> T;

    fn visit_unary(&mut self, op: &Token, expr: &Expr) -> T;

    fn visit_var_ref(&mut self, name: &Token) -> T;
}