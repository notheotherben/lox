use super::{Expr, ExprVisitor, Literal};

pub struct AstPrinter{}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary<'a>(&self, left: Expr<'a>, op: crate::lexer::Token<'a>, right: Expr<'a>) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
    }

    fn visit_grouping(&self, expr: Expr<'_>) -> String {
        format!("(group {})", self.visit_expr(expr))
    }

    fn visit_literal(&self, _lit: crate::lexer::Token<'_>, value: Literal) -> String {
        match value {
            Literal::Nil => "nil".to_string(),
            Literal::False => "false".to_string(),
            Literal::True => "true".to_string(),
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
    use crate::lexer::{Token, TokenType};

    use super::*;

    #[test]
    fn test_ast_printer() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(Token::new(TokenType::Minus, "-", 0, 0), Box::new(Expr::Literal(Token::new(TokenType::Number, "123", 1, 1), Literal::Number(123.))))),
            Token::new(TokenType::Star, "*", 0, 0),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Token::new(TokenType::Number, "45.67", 1, 1), Literal::Number(45.67))))),
        );

        let printer = AstPrinter{};
        let result = printer.visit_expr(expr);
        assert_eq!(result, "(* (- 123) (group 45.67))");
    }
}