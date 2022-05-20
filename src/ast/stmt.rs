use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
}

pub trait StmtVisitor<T>: ExprVisitor<T> {
    fn visit_stmt(&self, stmt: Stmt<'_>) -> T {
        match stmt {
            Stmt::Expression(expr) => self.visit_stmt_expr(expr),
            Stmt::Print(expr) => self.visit_print(expr),
        }
    }

    fn visit_print(&self, expr: Expr<'_>) -> T;

    fn visit_stmt_expr(&self, expr: Expr<'_>) -> T;
}