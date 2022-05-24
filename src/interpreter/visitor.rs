use std::rc::Rc;

use crate::{ast::{ExprVisitor, Literal, StmtVisitor, Stmt, Expr}, LoxError, lexer::Token, errors, interpreter::Class};

use super::{Value, Interpreter, Fun, class::Instance};

impl ExprVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(value)?;
        self.env.assign(ident.lexeme(), value.clone())?;
        Ok(value)
    }

    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, LoxError> {
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

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], close: &Token) -> Result<Value, LoxError> {
        match self.visit_expr(callee)? {
            Value::Class(class) => {
                if !args.is_empty() {
                    return Err(errors::user(
                        &format!("Class instantiation expects no arguments, but got {} at {}.", args.len(), close.location()),
                        "Do not provide any arguments to the class constructor."
                    ));
                }

                Ok(Value::Instance(Instance::new(class)))
            },
            Value::Function(fun) => {
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
            Value::Method(fun, instance) => {
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

    fn visit_fun_expr(&mut self, _token: &Token, params: &[Token], body: &[Stmt]) -> Result<Value, LoxError> {
        let fun = Fun::closure("@anonymous", params, body, self.env.clone());
        Ok(Value::Function(fun))
    }

    fn visit_get(&mut self, obj: &Expr, property: &Token) -> Result<Value, LoxError> {
        let obj = self.visit_expr(obj)?;
        match obj {
            Value::Instance(instance) => {
                instance.get(property)
            },
            _ => Err(errors::user(
                &format!("Attempted to access a property on a value which is not an instance {}.", property.location()),
                "Make sure that you are attempting to access a property on an instance or class object."
            ))
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, value: &Literal) -> Result<Value, LoxError> {
        Ok(value.into())
    }

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, LoxError> {
        let left = self.visit_expr(left)?;

        match op {
            Token::And(_) if left.is_truthy() => self.visit_expr(right),
            Token::Or(_) if !left.is_truthy() => self.visit_expr(right),
            _ => Ok(left)
        }
    }

    fn visit_set(&mut self, obj: &Expr, property: &Token, value: &Expr) -> Result<Value, LoxError> {
        let obj = self.visit_expr(obj)?;
        match obj {
            Value::Instance(mut instance) => {
                let value = self.visit_expr(value)?;
                instance.set(property, value.clone())?;
                Ok(value)
            },
            _ => Err(errors::user(
                &format!("Attempted to set a property on a value which is not an instance {}.", property.location()),
                "Make sure that you are attempting to set a property on an instance or class object."
            ))
        }
    }

    fn visit_unary(&mut self, op: &Token, expr: &Expr) -> Result<Value, LoxError> {
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

    fn visit_var_ref(&mut self, name: &Token) -> Result<Value, LoxError> {
        match self.env.get(name.lexeme()) {
            Some(value) => Ok(value),
            None => Err(errors::user(
                &format!("Variable `{}` is not defined.", name.lexeme()),
                "Define the variable before you attempt to reference it."
            ))
        }
    }
}

impl StmtVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_break(&mut self) -> Result<Value, LoxError> {
        self.breaking = true;
        Ok(Value::Nil)
    }

    fn visit_block(&mut self, stmts: &[Stmt]) -> Result<Value, LoxError> {
        let parent = self.env.clone();
        let mut result = Ok(Value::Nil);

        for stmt in stmts {
            if self.breaking {
                break;
            }

            if self.returning.is_some() {
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

    fn visit_class(&mut self, name: &Token, methods: &[Stmt]) -> Result<Value, LoxError> {
        self.env.define(name.lexeme(), Value::Nil);
        let mut class = Class::new(name.lexeme());

        for method in methods {
            match method {
                Stmt::Fun(name, params, body) => {
                    let fun = Fun::closure(name.lexeme(), params, body, self.env.clone());
                    class.define(name.lexeme(), fun);
                },
                _ => return Err(errors::system(
                    &format!("Unexpected statement in class definition: {:?}", method),
                    "This should not have occurred, please report the issue along with sample code."
                ))
            }
        }

        self.env.assign(name.lexeme(), Value::Class(Rc::new(class)))?;
        Ok(Value::Nil)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)?;

        Ok(Value::Nil)
    }

    fn visit_fun_def(&mut self, name: &Token, params: &[Token], body: &[Stmt]) -> Result<Value, LoxError> {
        let fun = Fun::closure(name.lexeme(), params, body, self.env.clone());
        self.env.define(name.lexeme(), Value::Function(fun));
        Ok(Value::Nil)
    }

    fn visit_if(&mut self, cond: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> Result<Value, LoxError> {
        let cond = self.visit_expr(cond)?;

        if cond.is_truthy() {
            self.visit_stmt(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)
        } else {
            Ok(Value::Nil)
        }
    }

    fn visit_print(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(Value::Nil)
    }

    fn visit_return(&mut self, _token: &Token, expr: Option<&Expr>) -> Result<Value, LoxError> {
        if let Some(expr) = expr {
            let value = self.visit_expr(expr)?;
            self.returning = Some(value);
        } else {
            self.returning = Some(Value::Nil);
        }

        Ok(Value::Nil)
    }

    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(expr)?;
        self.env = self.env.branch();
        self.env.define(name.lexeme(), value);
        Ok(Value::Nil)
    }

    fn visit_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Value, LoxError> {
        let mut cont = self.visit_expr(cond)?;
        while cont.is_truthy() && !self.breaking {
            self.visit_stmt(body)?;

            cont = self.visit_expr(cond)?;
        }

        self.breaking = false;
        Ok(Value::Nil)
    }
    
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{lexer::Scanner, ast::Parser};

    use super::*;

    #[test]
    fn test_basic_math() {
        let lexer = Scanner::new("10 - 12 / (2 * 3)");
        let tree = Parser::parse_expr(&mut lexer.filter_map(|x| x.ok())).expect("no errors");

        let mut interpreter = Interpreter::default();
        let result = interpreter.visit_expr(&tree).expect("no errors");
        assert_eq!(result, Value::Number(8.0));
    }

    #[test]
    fn test_recursive_functions() {
        let lexer = Scanner::new(r#"
            print "Defining fib function";
            fun fib(n) {
                if (n <= 2) return n;
                return fib(n - 1) + fib(n - 2);
            }

            print "Calling fib function";
            var result = fib(10);
            assert(result == 89, "Function should return the correct number, got " + result);
        "#);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        assert!(errs.is_empty(), "no errors");

        let mut interpreter = Interpreter::default();
        for err in interpreter.interpret(&tree) {
            panic!("{:?}", err);
        }
    }

    #[test]
    fn test_logical_closures() {
        let lexer = Scanner::new(r#"
            var scope = "outer";
            {
                fun test() {
                    print "Scope: " + scope;
                    assert(scope == "outer", "Scope should be outer");
                }

                test();

                var scope = "inner";
                test();
            }

            assert(scope == "outer", "The original scope variable should not be mutated.");
        "#);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        assert!(errs.is_empty(), "no errors");

        let mut interpreter = Interpreter::default();
        for err in interpreter.interpret(&tree) {
            panic!("{:?}", err);
        }
    }

    #[test]
    fn test_class_instantiation() {
        let lexer = Scanner::new(r#"
            class Foo {
                bar() {
                    return "Bar";
                }
            }
            print Foo;

            var foo = Foo();
            print foo;
        "#);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        for err in errs {
            panic!("{:?}", err);
        }

        let mut interpreter = Interpreter::default();
        for err in interpreter.interpret(&tree) {
            panic!("{:?}", err);
        }
    }

    #[test]
    fn test_property_access() {
        let lexer = Scanner::new(r#"
            class Foo {}
            print Foo;

            var foo = Foo();
            foo.bar = "baz";
            assert(foo.bar == "baz", "Property should be set and read correctly.");
        "#);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        for err in errs {
            panic!("{:?}", err);
        }

        let mut interpreter = Interpreter::default();
        for err in interpreter.interpret(&tree) {
            panic!("{:?}", err);
        }
    }

    #[test]
    fn test_class_methods() {
        let lexer = Scanner::new(r#"
            class Foo {
                bar() {
                    return "Bar";
                }
            }
            print Foo;

            var foo = Foo();
            assert(foo.bar() == "Bar", "Method should be called and return the correct value.");

            var bar = foo.bar;
            assert(bar() == "Bar", "Method should be called and return the correct value when it is raised to a variable.");
        "#);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        for err in errs {
            panic!("{:?}", err);
        }

        let mut interpreter = Interpreter::default();
        for err in interpreter.interpret(&tree) {
            panic!("{:?}", err);
        }
    }
}