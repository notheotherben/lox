use super::{Expr, ExprVisitor, Literal};

pub struct AstPrinter{}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary<'a>(&self, left: Expr<'a>, op: crate::lexer::Token<'a>, right: Expr<'a>) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
    }

    fn visit_grouping(&self, expr: Expr<'_>) -> String {
        format!("(group {})", self.visit_expr(expr))
    }

    fn visit_literal(&self, value: Literal) -> String {
        match value {
            Literal::Nil => "nil".to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(string) => format!("\"{}\"", string)
        }
    }

    fn visit_unary<'a>(&self, op: crate::lexer::Token<'a>, expr: Expr<'a>) -> String {
        format!("({} {})", op.lexeme(), self.visit_expr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Token, core::Location};

    use super::*;

    #[test]
    fn test_ast_printer() {
        let loc = Location::default();
        let expr = Expr::Binary(
            Box::new(Expr::Unary(Token::Minus(loc), Box::new(Expr::Literal(Literal::Number(123.))))),
            Token::Star(loc),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(45.67))))),
        );

        let printer = AstPrinter{};
        let result = printer.visit_expr(expr);
        assert_eq!(result, "(* (- 123) (group 45.67))");
    }
}