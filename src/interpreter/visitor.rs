use std::rc::Rc;

use crate::{ast::{ExprVisitor, Literal, StmtVisitor, Stmt, Expr, FunType}, LoxError, lexer::Token, errors, interpreter::Class, Loc};

use super::{Value, Interpreter, Fun, class::Instance};

impl ExprVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(value)?;
        self.env.assign(ident, value.clone())?;
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
                    (left, right) => Err(errors::runtime(
                        op.location(),
                        &format!("Invalid operands to binary operator: `{:?}` and `{:?}`", left, right),
                        "Provide either numbers or strings on both the left and right hand sides of the multiplication operator."
                    ))
                }
            },
            Token::Minus(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
                    (left, right) => Err(errors::runtime(
                        op.location(),
                        &format!("Invalid operands to binary operator: `{:?}` and `{:?}`", left, right),
                        "Provide numbers on both the left and right hand sides of the subtraction operator.",
                    ))
                }
            },
            Token::Slash(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
                    (left, right) => Err(errors::runtime(
                        op.location(),
                        &format!("Invalid operands to binary operator: `{:?}` and `{:?}`", left, right),
                        "Provide numbers on both the left and right hand sides of the division operator.",
                    ))
                }
            },
            Token::Star(_) => {
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
                    (left, right) => Err(errors::runtime(
                        op.location(),
                        &format!("Invalid operands to binary operator: `{:?}` and `{:?}`", left, right),
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
                let instance = Instance::new(class.clone());

                if let Some(init) = class.find_method("init") {
                    if init.arity() != args.len() {
                        return Err(errors::runtime(
                            close.location(),
                            &format!("Class init function expects {} arguments, but got {}.", init.arity(), args.len()),
                            "Provide the correct number of arguments to the class's `init` method."
                        ));
                    }

                    let mut evaluated_args = Vec::new();
                    for arg in args {
                        evaluated_args.push(self.visit_expr(arg)?);
                    }

                    init.bind(instance.clone(), close.location()).call(self, evaluated_args)?;
                } else if !args.is_empty() {
                    return Err(errors::runtime(
                        close.location(),
                        &format!("Class init function expects no arguments, but got {}.", args.len()),
                        "Provide the correct number of arguments to the class's `init` method."
                    ));
                }

                Ok(Value::Instance(instance))
            },
            Value::Function(fun) => {
                if args.len() != fun.arity() {
                    return Err(errors::runtime(
                        close.location(),
                        &format!("Expected {} arguments but got {}.", fun.arity(), args.len()),
                        "Provide the correct number of arguments to the function call."
                    ));
                }

                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.visit_expr(arg)?);
                }

                fun.call(self, evaluated_args)
            },
            other => Err(errors::runtime(
                close.location(),
                &format!("Attempted to invoke a value which is not a function or class `{}`.", other),
                "Make sure that you are attempting to call a function or class object."
            ))
        }
        
    }

    fn visit_fun_expr(&mut self, _loc: &Loc, params: &[Token], body: &[Stmt]) -> Result<Value, LoxError> {
        let fun = Fun::closure("@anonymous", params, body, self.env.clone());
        Ok(Value::Function(fun))
    }

    fn visit_get(&mut self, obj: &Expr, property: &Token) -> Result<Value, LoxError> {
        let obj = self.visit_expr(obj)?;
        match obj {
            Value::Class(class) => {
                class.get(property.lexeme()).ok_or_else(|| errors::runtime(
                    property.location(),
                    &format!("Class `{}` does not have a static property `{}`.", class.name(), property.lexeme()),
                    "Make sure that you are attempting to access a static property that exists on this class object."
                ))
            },
            Value::Instance(instance) => {
                instance.get(property)
            },
            _ => Err(errors::runtime(
                property.location(),
                "Attempted to access a property on a value which is not a class or instance of a class.",
                "Make sure that you are attempting to access a property on an instance or class object."
            ))
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, _loc: &Loc, value: &Literal) -> Result<Value, LoxError> {
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
            _ => Err(errors::runtime(
                property.location(),
                "Attempted to set a property on a value which is not a class instance.",
                "Make sure that you are attempting to set a property on an instance or class object."
            ))
        }
    }

    fn visit_super(&mut self, loc: &Loc, method: &Token) -> Result<Value, LoxError> {
        match (self.env.get("this"), self.env.get("super")) {
            (Some(Value::Instance(instance)), Some(Value::Class(superclass))) => {
                let loc = loc.with_sample(format!("super.{}", method.lexeme()));
                Ok(Value::Function(superclass.find_method(method.lexeme()).ok_or_else(|| errors::runtime(
                    loc.clone(),
                    &format!("Attempted to call a method `{}` on a superclass which does not have a method with that name.", method.lexeme()),
                    "Make sure that you are attempting to call a method on a superclass which exists."
                ))?.bind(instance, loc)))
            },
            _ => {
                Err(errors::runtime(
                    loc.with_sample("super"),
                    "Attempted to access `super` outside of a derived class's method.",
                    "Make sure that you are attempting to access `super` inside of a class method."
                ))
            }
        }
    }

    fn visit_this(&mut self, loc: &Loc) -> Result<Value, LoxError> {
        if let Some(instance) = self.env.get("this") {
            Ok(instance)
        } else {
            Err(errors::runtime(
                loc.with_sample("this"),
                "Attempted to access `this` outside a class method.",
                "Make sure that you are attempting to access `this` inside of a class method."
            ))
        }
    }

    fn visit_unary(&mut self, op: &Token, expr: &Expr) -> Result<Value, LoxError> {
        let right = self.visit_expr(expr)?;

        match op {
            Token::Minus(_) => {
                match right {
                    Value::Number(num) => Ok(Value::Number(-num)),
                    _ => Err(errors::runtime(
                        op.location(),
                        &format!("Invalid operand to unary operator: `{:?}`", right),
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
            None => Err(errors::runtime(
                name.location(),
                &format!("Variable `{}` is not defined.", name.lexeme()),
                "Define the variable before you attempt to reference it."
            ))
        }
    }
}

impl StmtVisitor<Result<Value, LoxError>> for Interpreter {
    fn visit_break(&mut self, _loc: &Loc) -> Result<Value, LoxError> {
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

    fn visit_class(&mut self, name: &Token, superclass: Option<&Expr>, statics: &[Stmt], methods: &[Stmt]) -> Result<Value, LoxError> {
        let superclass = match superclass {
            Some(Expr::Var(name)) => Some(self.env.get(name.lexeme()).ok_or_else(|| errors::runtime(
                name.location(),
                "Attempted to derive from a super-class which has not been defined.",
                "Define the superclass before you attempt to define a derived class."
            ))?),
            Some(_) => return Err(errors::runtime(
                name.location(),
                "Attempted to derive from a something that is not a valid super-class type.",
                "Make sure that you are providing a class name as the superclass."
            )),
            None => None,
        };

        let superclass = match superclass {
            Some(Value::Class(class)) => Some(class),
            None => None,
            _ => return Err(errors::runtime(
                name.location(),
                "Attempted to derive from a non-class object.",
                "Make sure that you are providing a class as the superclass."
            ))
        };
        
        self.env.define(name, Value::Nil);
        let mut class = Class::new(name.lexeme(), superclass);

        for method in statics {
            match method {
                Stmt::Fun(_ty, name, params, body) => {
                    let fun = Fun::closure(name.lexeme(), params, body, self.env.clone());
                    class.set(name.lexeme(), Value::Function(fun));
                },
                _ => panic!("We received an unexpected statement in a class definition: {:?}", method)
            }
        }

        let super_env = if let Some(superclass) = class.superclass() {
            let mut env = self.env.branch();
            env.define(&Token::Identifier(name.location(), "super".to_string()), Value::Class(superclass));
            env
        } else {
            self.env.clone()
        };

        for method in methods {
            match method {
                Stmt::Fun(FunType::Initializer, name, params, body) => {
                    let fun = Fun::initializer(name.lexeme(), params, body, super_env.clone());
                    class.define(name.lexeme(), fun);
                },
                Stmt::Fun(_, name, params, body) => {
                    let fun = Fun::closure(name.lexeme(), params, body, super_env.clone());
                    class.define(name.lexeme(), fun);
                },
                _ => return Err(errors::system(
                    &format!("Unexpected statement in class definition: {:?}", method),
                    "This should not have occurred, please report the issue along with sample code."
                ))
            }
        }

        self.env.assign(name, Value::Class(Rc::new(class)))?;
        Ok(Value::Nil)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)?;

        Ok(Value::Nil)
    }

    fn visit_fun_def(&mut self, _ty: FunType, name: &Token, params: &[Token], body: &[Stmt]) -> Result<Value, LoxError> {
        let fun = Fun::closure(name.lexeme(), params, body, self.env.clone());
        self.env.define(name, Value::Function(fun));
        Ok(Value::Nil)
    }

    fn visit_if(&mut self, _token: &Token, cond: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> Result<Value, LoxError> {
        let cond = self.visit_expr(cond)?;

        if cond.is_truthy() {
            self.visit_stmt(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)
        } else {
            Ok(Value::Nil)
        }
    }

    fn visit_print(&mut self, _loc: &Loc, expr: &Expr) -> Result<Value, LoxError> {
        let value = self.visit_expr(expr)?;
        writeln!(self.output, "{}", value)?;
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
        self.env.define(name, value);
        Ok(Value::Nil)
    }

    fn visit_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Value, LoxError> {
        let mut cont = self.visit_expr(cond)?;
        while cont.is_truthy() && !self.breaking && self.returning.is_none() {
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

    #[test]
    fn test_this_reference() {
        let lexer = Scanner::new(r#"
            class Cake {
                taste() {
                    var adjective = "delicious";
                    return "The " + this.flavor + " cake is " + adjective + "!";
                }
            }
            
            var cake = Cake();
            cake.flavor = "German chocolate";
            assert(cake.taste() == "The German chocolate cake is delicious!", "The method should be able to access its local context");
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
    fn class_constructors() {
        let lexer = Scanner::new(r#"
            class Cake {
                init(flavor) {
                    this.flavor = flavor;
                }
            }

            var cake = Cake("German chocolate");
            assert(cake.flavor == "German chocolate", "The constructor should be able to set the class properties.");

            assert(cake.init("Vanilla") == cake, "The constructor should return the instance if it is invoked directly.");
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
    fn class_static_methods() {
        let lexer = Scanner::new(r#"
            class Cake {
                class flavor() {
                    return "German chocolate";
                }
            }

            assert(Cake.flavor() == "German chocolate", "The static method should be addressable.");
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
    fn inheritance() {
        let lexer = Scanner::new(r#"
            class Cake {
                init(flavor) {
                    this.flavor = flavor;
                }
            }

            class VanillaCake < Cake {
                init(flavor) {
                    super.init(flavor);
                    this.flavor = "Vanilla " + flavor;
                }
            }

            var vanilla_cake = VanillaCake("German chocolate");
            assert(vanilla_cake.flavor == "Vanilla German chocolate", "The constructor should be able to set the class properties.");
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