use super::{Expr, ExprVisitor, Literal};

pub struct AstPrinter{}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary<'a>(&mut self, left: Expr<'a>, op: crate::lexer::Token<'a>, right: Expr<'a>) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
    }

    fn visit_grouping(&mut self, expr: Expr<'_>) -> String {
        format!("(group {})", self.visit_expr(expr))
    }

    fn visit_literal(&mut self, value: Literal) -> String {
        match value {
            Literal::Nil => "nil".to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(string) => format!("\"{}\"", string)
        }
    }

    fn visit_unary<'a>(&mut self, op: crate::lexer::Token<'a>, expr: Expr<'a>) -> String {
        format!("({} {})", op.lexeme(), self.visit_expr(expr))
    }

    fn visit_var_ref(&mut self, name: crate::lexer::Token<'_>) -> String {
        name.lexeme().to_string()
    }

    fn visit_assign(&mut self, ident: crate::lexer::Token<'_>, value: Expr<'_>) -> String {
        format!("(= {} {})", ident.lexeme(), self.visit_expr(value))
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

        let mut printer = AstPrinter{};
        let result = printer.visit_expr(expr);
        assert_eq!(result, "(* (- 123) (group 45.67))");
    }
}