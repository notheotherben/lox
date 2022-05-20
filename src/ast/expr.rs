use crate::lexer::Token;

use super::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(Literal),
    Unary(Token<'a>, Box<Expr<'a>>),
    Var(Token<'a>),
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: Expr<'_>) -> T {
        match expr {
            Expr::Assign(ident, value) => {
                self.visit_assign(ident, *value)
            },
            Expr::Binary(left, op, right) => {
                self.visit_binary(*left, op, *right)
            },
            Expr::Grouping(expr) => {
                self.visit_grouping(*expr)
            },
            Expr::Literal(value) => {
                self.visit_literal(value)
            },
            Expr::Unary(op, expr) => {
                self.visit_unary(op, *expr)
            },
            Expr::Var(name) => {
                self.visit_var_ref(name)
            },
        }
    }

    fn visit_assign(&mut self, ident: Token<'_>, value: Expr<'_>) -> T;

    fn visit_binary<'a>(&mut self, left: Expr<'a>, op: Token<'a>, right: Expr<'a>) -> T;

    fn visit_grouping(&mut self, expr: Expr<'_>) -> T;

    fn visit_literal(&mut self, value: Literal) -> T;

    fn visit_unary<'a>(&mut self, op: Token<'a>, expr: Expr<'a>) -> T;

    fn visit_var_ref(&mut self, name: Token<'_>) -> T;
}