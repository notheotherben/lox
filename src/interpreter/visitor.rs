use crate::{ast::{ExprVisitor, Literal, StmtVisitor}, LoxError, lexer::TokenType, errors};

pub struct Interpreter{}

impl ExprVisitor<Result<Literal, LoxError>> for Interpreter {
    fn visit_binary<'a>(&self, left: crate::ast::Expr<'a>, op: crate::lexer::Token<'a>, right: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;
        match op.token_type() {
            TokenType::BangEqual => {
                Ok(if left != right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::EqualEqual => {
                Ok(if left == right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::Greater => {
                Ok(if left > right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::Less => {
                Ok(if left < right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::GreaterEqual => {
                Ok(if left >= right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::LessEqual => {
                Ok(if left <= right {
                    Literal::True
                } else {
                    Literal::False
                })
            },
            TokenType::Plus => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left + right)),
                    (Literal::String(left), Literal::String(right)) => Ok(Literal::String(left + &right)),
                    (Literal::String(left), right) => Ok(Literal::String(format!("{}{}", left, right))),
                    (left, Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                    (left, right) => Err(errors::user_with_internal(
                        &format!("Invalid operands to binary operator `{}`: `{:?}` and `{:?}`", op.lexeme(), left, right),
                        "Provide either numbers or strings on both the left and right hand sides of the multiplication operator.",
                        op.location()
                    ))
                }
            },
            TokenType::Minus => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left - right)),
                    (left, right) => Err(errors::user_with_internal(
                        &format!("Invalid operands to binary operator `{}`: `{:?}` and `{:?}`", op.lexeme(), left, right),
                        "Provide numbers on both the left and right hand sides of the subtraction operator.",
                        op.location()
                    ))
                }
            },
            TokenType::Slash => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left / right)),
                    (left, right) => Err(errors::user_with_internal(
                        &format!("Invalid operands to binary operator `{}`: `{:?}` and `{:?}`", op.lexeme(), left, right),
                        "Provide numbers on both the left and right hand sides of the division operator.",
                        op.location()
                    ))
                }
            },
            TokenType::Star => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left * right)),
                    (left, right) => Err(errors::user_with_internal(
                        &format!("Invalid operands to binary operator `{}`: `{:?}` and `{:?}`", op.lexeme(), left, right),
                        "Provide numbers on both the left and right hand sides of the multiplication operator.",
                        op.location()
                    ))
                }
            },
            _ => panic!("We received an unexpected binary operator: {:?}", op)
        }
    }

    fn visit_unary<'a>(&self, op: crate::lexer::Token<'a>, expr: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
        let right = self.visit_expr(expr)?;

        match op.token_type() {
            TokenType::Minus => {
                match right {
                    Literal::Number(num) => Ok(Literal::Number(-num)),
                    _ => Err(errors::user_with_internal(
                        &format!("Invalid operand to unary operator `{}`: `{:?}`", op.lexeme(), right),
                        "Provide a number to the unary negation operator, or remove the minus sign.",
                        op.location()
                    ))
                }
            },
            TokenType::Bang => {
                match right {
                    Literal::False => Ok(Literal::True),
                    Literal::True => Ok(Literal::False),
                    Literal::Nil => Ok(Literal::True),
                    _ => Ok(Literal::False)
                }
            },
            _ => panic!("We received an unexpected unary operator: {:?}", op)
        }
    }

    fn visit_grouping(&self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&self, _lit: crate::lexer::Token<'_>, value: Literal) -> Result<Literal, LoxError> {
        Ok(value)
    }
}

impl StmtVisitor<Result<Literal, LoxError>> for Interpreter {
    fn visit_print(&self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(Literal::Nil)
    }

    fn visit_stmt_expr(&self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.visit_expr(expr)?;

        Ok(Literal::Nil)
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Scanner, ast::Parser};

    use super::*;

    #[test]
    fn test_basic_math() {
        let lexer = Scanner::new("10 - 12 / (2 * 3)");
        let tree = Parser::parse_expr(&mut lexer.filter_map(|x| x.ok())).expect("no errors");

        let interpreter = Interpreter{};
        let result = interpreter.visit_expr(tree).expect("no errors");
        assert_eq!(result, Literal::Number(8.0));
    }
}