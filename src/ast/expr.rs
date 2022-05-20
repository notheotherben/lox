use crate::lexer::Token;

use super::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(Token<'a>, Literal),
    Unary(Token<'a>, Box<Expr<'a>>)
}

pub trait ExprVisitor<T> {
    fn visit_expr(&self, expr: Expr<'_>) -> T {
        match expr {
            Expr::Binary(left, op, right) => {
                self.visit_binary(*left, op, *right)
            },
            Expr::Grouping(expr) => {
                self.visit_grouping(*expr)
            },
            Expr::Literal(lit, value) => {
                self.visit_literal(lit, value)
            },
            Expr::Unary(op, expr) => {
                self.visit_unary(op, *expr)
            }
        }
    }

    fn visit_binary<'a>(&self, left: Expr<'a>, op: Token<'a>, right: Expr<'a>) -> T;

    fn visit_grouping(&self, expr: Expr<'_>) -> T;

    fn visit_literal(&self, lit: Token<'_>, value: Literal) -> T;

    fn visit_unary<'a>(&self, op: Token<'a>, expr: Expr<'a>) -> T;
}