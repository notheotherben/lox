use std::{rc::Rc, sync::RwLock};

use crate::{ast::{ExprVisitor, Literal, StmtVisitor, Stmt}, LoxError, lexer::Token, errors};

use super::env::Environment;

#[derive(Default, Debug, Clone)]
pub struct Interpreter{
    env: Rc<RwLock<Environment>>,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: Vec<Stmt<'_>>) -> Result<(), LoxError> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl ExprVisitor<Result<Literal, LoxError>> for Interpreter {
    fn visit_binary<'a>(&mut self, left: crate::ast::Expr<'a>, op: crate::lexer::Token<'a>, right: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
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

    fn visit_unary<'a>(&mut self, op: crate::lexer::Token<'a>, expr: crate::ast::Expr<'a>) -> Result<Literal, LoxError> {
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
                Ok(Literal::Bool(!right.is_truthy()))
            },
            _ => panic!("We received an unexpected unary operator: {:?}", op)
        }
    }

    fn visit_grouping(&mut self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, value: Literal) -> Result<Literal, LoxError> {
        Ok(value)
    }

    fn visit_var_ref(&mut self, name: Token<'_>) -> Result<Literal, LoxError> {
        match self.env.read().unwrap().get(name.lexeme()) {
            Some(value) => Ok(value),
            None => Err(errors::user(
                &format!("Variable `{}` is not defined.", name.lexeme()),
                "Define the variable before you attempt to reference it."
            ))
        }
    }

    fn visit_assign(&mut self, ident: Token<'_>, value: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        let value = self.visit_expr(value)?;
        self.env.write().unwrap().assign(ident.lexeme(), value.clone())?;
        Ok(value)
    }

    fn visit_logical(&mut self, left: crate::ast::Expr<'_>, op: Token<'_>, right: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        let left = self.visit_expr(left)?;

        match op {
            Token::And(_) if left.is_truthy() => self.visit_expr(right),
            Token::Or(_) if !left.is_truthy() => self.visit_expr(right),
            _ => Ok(left)
        }
    }

    
}

impl StmtVisitor<Result<Literal, LoxError>> for Interpreter {
    fn visit_print(&mut self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(Literal::Nil)
    }

    fn visit_stmt_expr(&mut self, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        self.visit_expr(expr)?;

        Ok(Literal::Nil)
    }

    fn visit_var_def(&mut self, name: Token<'_>, expr: crate::ast::Expr<'_>) -> Result<Literal, LoxError> {
        let value = self.visit_expr(expr)?;
        self.env.write().unwrap().define(name.lexeme(), value);
        Ok(Literal::Nil)
    }

    fn visit_block(&mut self, stmts: Vec<crate::ast::Stmt<'_>>) -> Result<Literal, LoxError> {
        let parent = Rc::clone(&self.env);
        self.env = Environment::child(Rc::clone(&parent));

        let mut result = Ok(Literal::Nil);

        for stmt in stmts {
            if let Err(e) = self.visit_stmt(stmt) {
                result = Err(e);
                break;
            }
        }

        self.env = parent;
        result
    }

    fn visit_if(&mut self, cond: crate::ast::Expr<'_>, then_branch: crate::ast::Stmt<'_>, else_branch: Option<crate::ast::Stmt<'_>>) -> Result<Literal, LoxError> {
        let cond = self.visit_expr(cond)?;

        if cond.is_truthy() {
            self.visit_stmt(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)
        } else {
            Ok(Literal::Nil)
        }
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

        let mut interpreter = Interpreter::default();
        let result = interpreter.visit_expr(tree).expect("no errors");
        assert_eq!(result, Literal::Number(8.0));
    }
}