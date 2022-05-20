use crate::{ast::{ExprVisitor, Literal, StmtVisitor}, LoxError, lexer::Token, errors};

use super::env::Environment;

#[derive(Default, Debug, Clone)]
pub struct Interpreter{
    env: Environment,
}

impl ExprVisitor<Result<Literal, LoxError>> for Interpreter {
    fn visit_binary<'a>(&self, left: crate::ast::Expr<'a>, op: crate::lexer::Token<'a>, right: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;
        match op {
            Token::BangEqual(_) => {
                Ok(if left != right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::EqualEqual(_) => {
                Ok(if left == right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::Greater(_) => {
                Ok(if left > right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::Less(_) => {
                Ok(if left < right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::GreaterEqual(_) => {
                Ok(if left >= right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::LessEqual(_) => {
                Ok(if left <= right {
                    Literal::Bool(true)
                } else {
                    Literal::Bool(false)
                })
            },
            Token::Plus(_) => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left + right)),
                    (Literal::String(left), Literal::String(right)) => Ok(Literal::String(left + &right)),
                    (Literal::String(left), right) => Ok(Literal::String(format!("{}{}", left, right))),
                    (left, Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide either numbers or strings on both the left and right hand sides of the multiplication operator."
                    ))
                }
            },
            Token::Minus(_) => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left - right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the subtraction operator.",
                    ))
                }
            },
            Token::Slash(_) => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left / right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the division operator.",
                    ))
                }
            },
            Token::Star(_) => {
                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left * right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the multiplication operator.",
                    ))
                }
            },
            _ => panic!("We received an unexpected binary operator: {:?}", op)
        }
    }

    fn visit_unary<'a>(&self, op: crate::lexer::Token<'a>, expr: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
        let right = self.visit_expr(expr)?;

        match op {
            Token::Minus(_) => {
                match right {
                    Literal::Number(num) => Ok(Literal::Number(-num)),
                    _ => Err(errors::user(
                        &format!("Invalid operand to unary operator {}: `{:?}`", op, right),
                        "Provide a number to the unary negation operator, or remove the minus sign."
                    ))
                }
            },
            Token::Bang(_) => {
                match right {
                    Literal::Bool(b) => Ok(Literal::Bool(!b)),
                    Literal::Nil => Ok(Literal::Bool(true)),
                    _ => Ok(Literal::Bool(false))
                }
            },
            _ => panic!("We received an unexpected unary operator: {:?}", op)
        }
    }

    fn visit_grouping(&self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&self, value: Literal) -> Result<Literal, LoxError> {
        Ok(value)
    }

    fn visit_var_ref(&self, name: Token<'_>) -> Result<Literal, LoxError> {
        match self.env.get(name.lexeme()) {
            Some(value) => Ok(value.clone()),
            None => Err(errors::user(
                &format!("Variable `{}` is not defined.", name.lexeme()),
                "Define the variable before you attempt to reference it."
            ))
        }
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

    fn visit_var_def(&mut self, name: Token<'_>, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.env.define(name.lexeme(), self.visit_expr(expr)?);
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

        let interpreter = Interpreter::default();
        let result = interpreter.visit_expr(tree).expect("no errors");
        assert_eq!(result, Literal::Number(8.0));
    }
}