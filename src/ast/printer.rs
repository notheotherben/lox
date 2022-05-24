use super::{Expr, ExprVisitor, Literal, StmtVisitor, Stmt};

pub struct AstPrinter{}

impl ExprVisitor<String> for AstPrinter {
    fn visit_assign(&mut self, ident: &crate::lexer::Token, value: &Expr) -> String {
        format!("(= {} {})", ident.lexeme(), self.visit_expr(value))
    }

    fn visit_binary(&mut self, left: &Expr, op: &crate::lexer::Token, right: &Expr) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
    }

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], _close: &crate::lexer::Token) -> String {
        let mut s = format!("(call {}", self.visit_expr(callee));
        for arg in args {
            s.push(' ');
            s.push_str(&self.visit_expr(arg));
        }
        s.push(')');
        s
    }

    fn visit_get(&mut self, obj: &Expr, name: &crate::lexer::Token) -> String {
        format!("{}.{}", self.visit_expr(obj), name.lexeme())
    }

    fn visit_fun_expr(&mut self, _token: &crate::lexer::Token, params: &[crate::lexer::Token], body: &[Stmt]) -> String {
        let mut s = "(fun @anonymous".to_string();
        for param in params {
            s.push(' ');
            s.push_str(param.lexeme());
        }
        s.push(' ');
        s.push_str(&self.visit_block(body));
        s.push(')');
        s
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        format!("(group {})", self.visit_expr(expr))
    }

    fn visit_literal(&mut self, value: &Literal) -> String {
        match value {
            Literal::Nil => "nil".to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(string) => format!("\"{}\"", string),
        }
    }

    fn visit_logical(&mut self, left: &Expr, op: &crate::lexer::Token, right: &Expr) -> String {
        format!("({} {} {})", op.lexeme(), self.visit_expr(left), self.visit_expr(right))
    }

    fn visit_set(&mut self, obj: &Expr, name: &crate::lexer::Token, value: &Expr) -> String {
        format!("(set {}.{} {})", self.visit_expr(obj), name.lexeme(), self.visit_expr(value))
    }

    fn visit_this(&mut self, _token: &crate::lexer::Token) -> String {
        "this".to_string()
    }

    fn visit_unary(&mut self, op: &crate::lexer::Token, expr: &Expr) -> String {
        format!("({} {})", op.lexeme(), self.visit_expr(expr))
    }

    fn visit_var_ref(&mut self, name: &crate::lexer::Token) -> String {
        name.lexeme().to_string()
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_break(&mut self) -> String {
        "break".to_string()
    }

    fn visit_block(&mut self, stmts: &[Stmt]) -> String {
        let mut result = String::new();
        result.push_str("(block");
        for stmt in stmts {
            result.push(' ');
            result.push_str(&self.visit_stmt(stmt));
        }
        result.push(')');
        result
    }

    fn visit_class(&mut self, name: &crate::lexer::Token, methods: &[Stmt]) -> String {
        let mut result = String::new();
        result.push_str("(class ");
        result.push_str(name.lexeme());
        for method in methods {
            result.push(' ');
            result.push_str(&self.visit_stmt(method));
        }
        result.push(')');
        result
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> String {
        format!("({})", self.visit_expr(expr))
    }

    fn visit_fun_def(&mut self, name: &crate::lexer::Token, params: &[crate::lexer::Token], body: &[Stmt]) -> String {
        let mut result = String::new();
        result.push_str("(fun ");
        result.push_str(name.lexeme());
        for param in params {
            result.push(' ');
            result.push_str(param.lexeme());
        }
        result.push(' ');
        result.push_str(&self.visit_block(body));
        result.push(')');
        result
    }

    fn visit_if(&mut self, cond: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> String {
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

    fn visit_print(&mut self, expr: &Expr) -> String {
        format!("(print {})", self.visit_expr(expr))
    }

    fn visit_return(&mut self, _token: &crate::lexer::Token, expr: Option<&Expr>) -> String {
        let mut result = String::new();
        result.push_str("(return");
        if let Some(expr) = expr {
            result.push(' ');
            result.push_str(&self.visit_expr(expr));
        }
        result.push(')');
        result
    }

    fn visit_var_def(&mut self, name: &crate::lexer::Token, expr: &Expr) -> String {
        format!("(var {} {})", name.lexeme(), self.visit_expr(expr))
    }

    fn visit_while(&mut self, cond: &Expr, body: &Stmt) -> String {
        format!("(while {} {})", self.visit_expr(cond), self.visit_stmt(body))
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
        let result = printer.visit_expr(&expr);
        assert_eq!(result, "(* (- 123) (group 45.67))");
    }
}