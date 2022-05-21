use super::{Expr, ExprVisitor, Literal, StmtVisitor, Stmt};

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

    fn visit_logical(&mut self, left: Expr<'_>, op: crate::lexer::Token<'_>, right: Expr<'_>) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
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

impl StmtVisitor<String> for AstPrinter {
    fn visit_print(&mut self, expr: Expr<'_>) -> String {
        format!("(print {})", self.visit_expr(expr))
    }

    fn visit_stmt_expr(&mut self, expr: Expr<'_>) -> String {
        format!("({})", self.visit_expr(expr))
    }

    fn visit_var_def(&mut self, name: crate::lexer::Token<'_>, expr: Expr<'_>) -> String {
        format!("(var {} {})", name.lexeme(), self.visit_expr(expr))
    }

    fn visit_block(&mut self, stmts: Vec<Stmt<'_>>) -> String {
        let mut result = String::new();
        result.push_str("(block");
        for stmt in stmts {
            result.push(' ');
            result.push_str(&self.visit_stmt(stmt));
        }
        result.push(')');
        result
    }

    fn visit_if(&mut self, cond: Expr<'_>, then_branch: Stmt<'_>, else_branch: Option<Stmt<'_>>) -> String {
        let mut result = String::new();
        result.push_str("(if ");
        result.push_str(&self.visit_expr(cond));
        result.push(' ');
        result.push_str(&self.visit_stmt(then_branch));
        if let Some(else_branch) = else_branch {
            result.push(' ');
            result.push_str(&self.visit_stmt(else_branch));
        }
        result.push(')');
        result
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