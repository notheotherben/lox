use std::rc::Rc;

use crate::{ast::{ExprVisitor, Literal, StmtVisitor}, LoxError, lexer::Token, errors};

use super::{env::Environment, Value, Interpreter};

impl ExprVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_binary(&mut self, left: crate::ast::Expr, op: crate::lexer::Token, right: crate::ast::Expr) -> Result<Value, LoxError> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;
        match op {
            Token::BangEqual(_) => {
                Ok(if left != right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::EqualEqual(_) => {
                Ok(if left == right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::Greater(_) => {
                Ok(if left > right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::Less(_) => {
                Ok(if left < right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::GreaterEqual(_) => {
                Ok(if left >= right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::LessEqual(_) => {
                Ok(if left <= right {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                })
            },
            Token::Plus(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
                    (Value::String(left), Value::String(right)) => Ok(Value::String(left + &right)),
                    (Value::String(left), right) => Ok(Value::String(format!("{}{}", left, right))),
                    (left, Value::String(right)) => Ok(Value::String(format!("{}{}", left, right))),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide either numbers or strings on both the left and right hand sides of the multiplication operator."
                    ))
                }
            },
            Token::Minus(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the subtraction operator.",
                    ))
                }
            },
            Token::Slash(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the division operator.",
                    ))
                }
            },
            Token::Star(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
                    (left, right) => Err(errors::user(
                        &format!("Invalid operands to binary operator {}: `{:?}` and `{:?}`", op, left, right),
                        "Provide numbers on both the left and right hand sides of the multiplication operator.",
                    ))
                }
            },
            _ => panic!("We received an unexpected binary operator: {:?}", op)
        }
    }

    fn visit_call(&mut self, callee: crate::ast::Expr, args: Vec<crate::ast::Expr>, close: Token) -> Result<Value, LoxError> {
        match self.visit_expr(callee)? {
            Value::Callable(fun) => {
                if args.len() != fun.arity() {
                    return Err(errors::user(
                        &format!("Expected {} arguments but got {} at {}.", fun.arity(), args.len(), close.location()),
                        "Provide the correct number of arguments to the function call."
                    ));
                }

                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.visit_expr(arg)?);
                }

                fun.call(self, evaluated_args)
            },
            other => Err(errors::user(
                &format!("Attempted to invoke a value which is not a function or class `{}` at {}.", other, close.location()),
                "Make sure that you are attempting to call a function or class object."
            ))
        }
        
    }

    fn visit_unary(&mut self, op: crate::lexer::Token, expr: crate::ast::Expr) -> Result<Value, LoxError> {
        let right = self.visit_expr(expr)?;

        match op {
            Token::Minus(_) => {
                match right {
                    Value::Number(num) => Ok(Value::Number(-num)),
                    _ => Err(errors::user(
                        &format!("Invalid operand to unary operator {}: `{:?}`", op, right),
                        "Provide a number to the unary negation operator, or remove the minus sign."
                    ))
                }
            },
            Token::Bang(_) => {
                Ok(Value::Bool(!right.is_truthy()))
            },
            _ => panic!("We received an unexpected unary operator: {:?}", op)
        }
    }

    fn visit_grouping(&mut self, expr: crate::ast::Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, value: Literal) -> Result<Value, LoxError> {
        Ok(value.into())
    }

    fn visit_var_ref(&mut self, name: Token) -> Result<Value, LoxError> {
        match self.env.read().unwrap().get(name.lexeme()) {
            Some(value) => Ok(value),
            None => Err(errors::user(
                &format!("Variable `{}` is not defined.", name.lexeme()),
                "Define the variable before you attempt to reference it."
            ))
        }
    }

    fn visit_assign(&mut self, ident: Token, value: crate::ast::Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(value)?;
        self.env.write().unwrap().assign(ident.lexeme(), value.clone())?;
        Ok(value)
    }

    fn visit_logical(&mut self, left: crate::ast::Expr, op: Token, right: crate::ast::Expr) -> Result<Value, LoxError> {
        let left = self.visit_expr(left)?;

        match op {
            Token::And(_) if left.is_truthy() => self.visit_expr(right),
            Token::Or(_) if !left.is_truthy() => self.visit_expr(right),
            _ => Ok(left)
        }
    }

    
}

impl StmtVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_print(&mut self, expr: crate::ast::Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(Value::Nil)
    }

    fn visit_break(&mut self) -> Result<Value, LoxError> {
        self.breaking = true;
        Ok(Value::Nil)
    }

    fn visit_stmt_expr(&mut self, expr: crate::ast::Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)?;

        Ok(Value::Nil)
    }

    fn visit_var_def(&mut self, name: Token, expr: crate::ast::Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(expr)?;
        self.env.write().unwrap().define(name.lexeme(), value);
        Ok(Value::Nil)
    }

    fn visit_block(&mut self, stmts: Vec<crate::ast::Stmt>) -> Result<Value, LoxError> {
        let parent = Rc::clone(&self.env);
        self.env = Environment::child(Rc::clone(&parent));

        let mut result = Ok(Value::Nil);

        for stmt in stmts {
            if self.breaking {
                break;
            }

            if let Err(e) = self.visit_stmt(stmt) {
                result = Err(e);
                break;
            }
        }

        self.env = parent;
        result
    }

    fn visit_if(&mut self, cond: crate::ast::Expr, then_branch: crate::ast::Stmt, else_branch: Option<crate::ast::Stmt>) -> Result<Value, LoxError> {
        let cond = self.visit_expr(cond)?;

        if cond.is_truthy() {
            self.visit_stmt(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)
        } else {
            Ok(Value::Nil)
        }
    }

    fn visit_while(&mut self, cond: crate::ast::Expr, body: crate::ast::Stmt) -> Result<Value, LoxError> {
        // TODO: Figure out how to avoid the need to clone cond/body here
        let mut cont = self.visit_expr(cond.clone())?;
        while cont.is_truthy() && !self.breaking {
            self.visit_stmt(body.clone())?;

            cont = self.visit_expr(cond.clone())?;
        }

        self.breaking = false;
        Ok(Value::Nil)
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
        assert_eq!(result, Value::Number(8.0));
    }
}