use std::iter::Peekable;

use crate::{errors, lexer::Token, LoxError};

use super::{Expr, Literal, Stmt};

pub struct Parser {}

struct ParseContext<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    loop_depth: usize,
}

// Macros which make it easier to implement certain common parts of the parser.
macro_rules! rd_term {
    ($name:ident($context:ident) :=> $ret:ty : $body:expr) => {
        fn $name<'a, T: Iterator<Item = Token>>(
            $context: &mut ParseContext<T>,
        ) -> Result<$ret, LoxError> {
            $body
        }
    };

    ($name:ident := $left:ident ( $($token:ident)|+ $right:ident )* => Vec<Token>) => {
        rd_term!($name(context) :=> Vec<Token> : {
            let mut items = vec![rd_consume!(context, $left, format!("Expected a {} token here", stringify!($left)), "Make sure that you have provided a valid token here.")?];
    
            while matches!(context.tokens.peek(), Some($(Token::$token(..))|+)) {
                context.tokens.next();
                items.push(rd_consume!(context, $right, format!("Expected a {} token here", stringify!($right)), "Make sure that you have provided a valid token here.")?);
            }

            Ok(items)
        });
    };

    ($name:ident := $left:ident ( $($token:ident)|+ $right:ident )* => Vec<$ret:ty>) => {
        rd_term!($name(context) :=> Vec<$ret> : {
            let mut items = vec![Self::$left(context)?];
    
            while matches!(context.tokens.peek(), Some($(Token::$token(..))|+)) {
                context.tokens.next();
                match Self::$right(context) {
                    Ok(item) => items.push(item),
                    Err(err) => {
                        Self::synchronize(context);
                        return Err(err);
                    }
                }
            }

            Ok(items)
        });
    };

    ($name:ident := $item:ident* ... $($term:ident)|+  => Vec<$ret:ty>) => {
        rd_term!($name(context) :=> Vec<$ret> : {
            let mut items = Vec::new();
    
            while !matches!(context.tokens.peek(), Some($(Token::$term(..))|+)) {
                match Self::$item(context) {
                    Ok(item) => items.push(item),
                    Err(err) => {
                        Self::synchronize(context);
                        return Err(err);
                    }
                }
            }

            Ok(items)
        });
    };

    ($name:ident := $left:ident ( $($token:ident)|+ $right:ident )* => binary) => {
        rd_term!($name(context) :=> Expr : {
            let left = Self::$left(context)?;
    
            if !matches!(context.tokens.peek(), Some($(Token::$token(..))|+)) {
                return Ok(left)
            }
    
            let op = context.tokens.next().unwrap();
            let right = Self::$right(context)?;
            Ok(Expr::Binary(
                Box::new(left),
                op,
                Box::new(right),
            ))
        });
    };

    ($name:ident := ($($token:ident)|+ $right:ident) | $fallback:ident => unary) => {
        rd_term!($name(context) :=> Expr : {
            if !matches!(context.tokens.peek(), Some($(Token::$token(_))|+)) {
                return Self::$fallback(context);
            };
    
            let op = context.tokens.next().unwrap();
            let right = Self::$right(context)?;
            Ok(Expr::Unary(op, Box::new(right)))
        });
    };
}

macro_rules! rd_matches {
    ($context:ident, $($token:ident)|+) => {
        if matches!($context.tokens.peek(), $(Some(Token::$token(..)))|+) {
            Some($context.tokens.next().unwrap())
        } else {
            None
        }
    };
}

macro_rules! rd_consume {
    ($context:ident, $($id:ident@$token:ident)|+ => $ok:expr, $msg:expr, $advice:expr) => {
        match $context.tokens.next() {
            Some($($id@Token::$token(..))|+) => Ok($ok),
            Some(close_paren) => Err(errors::user(
                &format!("{}, but got {} instead.", $msg, close_paren),
                $advice
            )),
            None => Err(errors::user(
                &format!("{}, but reached the end of the file instead.", $msg),
                $advice
            )),
        }
    };

    ($context:ident, $($token:ident)|+ => $ok:expr, $msg:expr, $advice:expr) => {
        match $context.tokens.next() {
            Some($(Token::$token(..))|+) => Ok($ok),
            Some(close_paren) => Err(errors::user(
                &format!("{}, but got {} instead.", $msg, close_paren),
                $advice
            )),
            None => Err(errors::user(
                &format!("{}, but reached the end of the file instead.", $msg),
                $advice
            )),
        }
    };

    ($context:ident, $($token:ident)|+, $msg:expr, $advice:expr) => {
        rd_consume!($context, $(token@$token)|+ => token, $msg, $advice)
    };
}

impl Parser {
    pub fn parse<T: Iterator<Item = Token>>(
        tokens: &mut T,
    ) -> (Vec<Stmt>, Vec<LoxError>) {
        let mut context = ParseContext {
            tokens: tokens.peekable(),
            loop_depth: 0,
        };

        let mut stmts = Vec::new();
        let mut errs = Vec::new();

        while context.tokens.peek().is_some() {
            match Self::declaration(&mut context) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    Self::synchronize(&mut context);
                    errs.push(err);
                },
            }
        }

        (stmts, errs)
    }

    pub fn parse_expr<T: Iterator<Item = Token>>(
        tokens: &mut T,
    ) -> Result<Expr, LoxError> {
        let mut context = ParseContext {
            tokens: tokens.peekable(),
            loop_depth: 1
        };
        Self::expression(&mut context)
    }

    rd_term!(declaration(context) :=> Stmt : {
        match rd_matches!(context, Class | Fun | Var) {
            Some(Token::Class(..)) => {
                let name = rd_consume!(context, Identifier, "Expected a class name here", "Make sure that you have provided a valid class name here.")?;
                rd_consume!(context, LeftBrace, "Expected a left brace '{' after the class name", "Make sure that you have provided a left brace '{' here.")?;
                let methods = Self::functions(context)?;
                rd_consume!(context, RightBrace, "Expected a right brace '}' after the class body", "Make sure that you have provided a right brace '}' here.")?;

                Ok(Stmt::Class(name, methods))
            },
            Some(Token::Fun(..)) => {
                Self::function(context)
            },
            Some(Token::Var(..)) => {
                rd_consume!(context, ident@Identifier => {
                    let init = if let Some(Token::Equal(_)) = context.tokens.peek() {
                        context.tokens.next();
                        Self::expression(context)?
                    } else {
                        Expr::Literal(Literal::Nil)
                    };
        
                    rd_consume!(
                        context,
                        Semicolon => Stmt::Var(ident, init),
                        "Expected ';' after variable declaration",
                        "Make sure that you have a semicolon after the variable declaration.")
                    },
                    "Expected an identifier to be provided after 'var'",
                    "Provide a variable name after the `var` keyword.")?
            },
            _ => Self::statement(context)
        } 
    });

    rd_term!(functions := function* ...RightBrace => Vec<Stmt>);

    rd_term!(function(context) :=> Stmt : {
        rd_consume!(context, ident@Identifier => {
            rd_consume!(context, LeftParen, "Expected '(' after function name", "Make sure that you have a '(' after the function name.")?;
            
            let params = if matches!(context.tokens.peek(), Some(Token::RightParen(..))) {
                Vec::new()
            } else {
                Self::parameters(context)?   
            };

            rd_consume!(context, RightParen, "Expected ')' after the function parameters", "Make sure that you have a ')' after the function parameters.")?;

            rd_consume!(context, LeftBrace, "Expected '{' after the function parameters", "Make sure that you have a '{' after the function parameters.")?;

            let body = Self::block(context)?;

            Ok(Stmt::Fun(ident, params, body))
        },
        "Expected a function name to be provided when declaring a new function",
        "Provide a function name here, followed by the function's parameters and body.")?
    });

    rd_term!(parameters := Identifier (Comma Identifier)* => Vec<Token>);

    rd_term!(statement(context) :=> Stmt : {
        let stmt = match context.tokens.peek() {
            Some(Token::For(_)) => {
                context.tokens.next();
                context.loop_depth += 1;
                let stmt = Self::for_statement(context);
                context.loop_depth -= 1;
                return stmt
            },
            Some(Token::If(_)) => {
                context.tokens.next();
                return Self::if_statement(context)
            },
            Some(Token::LeftBrace(_)) => {
                context.tokens.next();
                return Ok(Stmt::Block(Self::block(context)?))
            },
            Some(Token::While(_)) => {
                context.tokens.next();
                context.loop_depth += 1;
                let stmt = Self::while_statement(context);
                context.loop_depth -= 1;
                return stmt
            },
            Some(Token::Return(_)) => {
                let ret = context.tokens.next().unwrap();
                let expr = if let Some(Token::Semicolon(_)) = context.tokens.peek() {
                    None
                } else {
                    Some(Self::expression(context)?)
                };

                Stmt::Return(ret, expr)
            },
            Some(Token::Break(_)) => {
                context.tokens.next();

                if context.loop_depth == 0 {
                    return Err(errors::user(
                        "Cannot use 'break' outside of a loop.",
                        "Make sure that you are within the bounds of a `while` or `for` loop when using the `break` keyword."
                    ));
                }

                Stmt::Break
            },
            Some(Token::Print(_)) => {
                context.tokens.next();
                let expr = Self::expression(context)?;
                Stmt::Print(expr)
            },
            _ => {
                let expr = Self::expression(context)?;
                Stmt::Expression(expr)
            }
        };

        rd_consume!(context, Semicolon => stmt, "Expected ';' after expression", "Make sure that you have a semicolon at the end of your previous expression.")
    });

    rd_term!(if_statement(context) :=> Stmt : {
        rd_consume!(context, LeftParen, "Expected an opening parenthesis `(` after the `if` keyword", "Make sure you have an opening parenthesis `(` after the `if` keyword.")?;

        let condition = Self::expression(context)?;

        rd_consume!(context, RightParen, "Expected a closing parenthesis `)` after the `if` keyword's condition", "Make sure you have a closing parenthesis `)` after the `if` keyword's condition.")?;
        
        let then_branch = Self::statement(context)?;

        Ok(Stmt::If(
            condition,
            Box::new(then_branch),
            if rd_matches!(context, Else).is_some() {
                let else_branch = Self::statement(context)?;
                 Some(Box::new(else_branch))
            } else {
                None
            }
        ))
    });

    rd_term!(for_statement(context) :=> Stmt : {
        rd_consume!(context, LeftParen, "Expected an opening parenthesis `(` after the `for` keyword", "Make sure you have an opening parenthesis `(` after the `for` keyword.")?;

        let init = match context.tokens.peek() {
            Some(Token::Semicolon(_)) => {
                context.tokens.next();
                None
            },
            Some(Token::Var(_)) => {
                Some(Self::declaration(context)?)
            },
            _ => {
                let expr = Stmt::Expression(Self::expression(context)?);
                rd_consume!(context, Semicolon, "Expected a semicolon after the initializer", "Make sure you have a semicolon after the initializer.")?;
                Some(expr)
            }
        };

        let cond = if rd_matches!(context, Semicolon).is_some() {
            None
        } else {
            let cond = Self::expression(context)?;
            rd_consume!(context, Semicolon, "Expected a semicolon after the condition", "Make sure you have a semicolon after the condition.")?;
            Some(cond)
        };

        let incr = if rd_matches!(context, RightParen).is_some() {
            None
        } else {
            let incr = Self::expression(context)?;
            rd_consume!(context, RightParen, "Expected a closing parenthesis `)` after the `for` keyword's condition", "Make sure you have a closing parenthesis `)` after the `for` keyword's condition.")?;
            Some(incr)
        };

        let mut body = Self::statement(context)?;

        if let Some(incr) = incr {
            body = Stmt::Block(vec![body, Stmt::Expression(incr)]);
        }

        body = Stmt::While(cond.unwrap_or(Expr::Literal(Literal::Bool(true))), Box::new(body));

        if let Some(init) = init {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    });

    rd_term!(while_statement(context) :=> Stmt : {
        rd_consume!(context, LeftParen, "Expected an opening parenthesis `(` after the `while` keyword", "Make sure you have an opening parenthesis `(` after the `while` keyword.")?;

        let condition = Self::expression(context)?;

        rd_consume!(context, RightParen, "Expected a closing parenthesis `)` after the `while` keyword's condition", "Make sure you have a closing parenthesis `)` after the `while` keyword's condition.")?;
        

        let body = Self::statement(context)?;

        Ok(Stmt::While(condition, Box::new(body)))
    });

    rd_term!(block(context) :=> Vec<Stmt> : {
        let mut stmts = Vec::new();

        while !matches!(context.tokens.peek(), Some(Token::RightBrace(_)) | None) {
            match Self::declaration(context) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    Self::synchronize(context);
                    return Err(err)
                },
            }
        }

        rd_consume!(context, RightBrace => stmts, "Expected a closing brace `}` after the block", "Make sure you have a closing brace `}` after the block.")
    });

    rd_term!(expression(context) :=> Expr : {
        if let Some(fun) = rd_matches!(context, Fun) {
            rd_consume!(context, LeftParen, "Expected '(' after function name", "Make sure that you have a '(' after the function name.")?;
                    
            let params = if matches!(context.tokens.peek(), Some(Token::RightParen(..))) {
                Vec::new()
            } else {
                Self::parameters(context)?   
            };

            rd_consume!(context, RightParen, "Expected ')' after the function parameters", "Make sure that you have a ')' after the function parameters.")?;

            rd_consume!(context, LeftBrace, "Expected '{' after the function parameters", "Make sure that you have a '{' after the function parameters.")?;

            let body = Self::block(context)?;

            Ok(Expr::Fun(fun, params, body))
        } else {
            Self::assignment(context)
        }
    });

    rd_term!(assignment(context) :=> Expr : {
        let expr = Self::or(context)?;

        if let Some(equals) = rd_matches!(context, Equal) {
            let value = Self::assignment(context)?;

            match expr {
                Expr::Var(name) => Ok(Expr::Assign(name, Box::new(value))),
                _ => Err(errors::user(
                    &format!("Expected a variable identifier to be assigned to, but got {:?} instead at {}.", expr, equals),
                    "Make sure that you provide the name of a variable to assign to."
                )),
            }
         } else {
            Ok(expr)
        }
    });

    rd_term!(or := and (Or and)* => binary);

    rd_term!(and := equality (And equality)* => binary);

    rd_term!(equality := comparison (BangEqual|EqualEqual equality)* => binary);

    rd_term!(comparison := term (Greater | GreaterEqual | Less | LessEqual equality)* => binary);

    rd_term!(term := factor (Minus | Plus equality)* => binary);

    rd_term!(factor := unary (Star | Slash equality)* => binary);

    rd_term!(unary := (Bang|Minus term)| call => unary);

    rd_term!(call(context) :=> Expr : {
        let mut expr = Self::primary(context)?;

        while matches!(context.tokens.peek(), Some(Token::LeftParen(_))) {
            context.tokens.next();
            let args = if matches!(context.tokens.peek(), Some(Token::RightParen(..))) {
                Vec::new()
            } else {
                Self::arguments(context)?
            };

            let call = rd_consume!(context, call@RightParen => call, "Expected a closing parenthesis `)` after the function call's arguments", "Make sure you have a closing parenthesis `)` after the function call's arguments.")?;

            if args.len() >= 255 {
                return Err(errors::user(
                    &format!("Found {} arguments in function call at {}, which is more than the 255 argument limit.", args.len(), call),
                    "Make sure that you don't have more than 255 arguments in a function call and try refactoring your code to accept an array or object of arguments."
                ))
            }

            expr = Expr::Call(Box::new(expr), args, call);
        }

        Ok(expr)
    });

    rd_term!(arguments := expression (Comma expression)* => Vec<Expr>);

    rd_term!(primary(context) :=> Expr : {
        match context.tokens.next() {
            Some(Token::False(_)) => Ok(Expr::Literal(Literal::Bool(false))),
            Some(Token::True(_)) => Ok(Expr::Literal(Literal::Bool(true))),
            Some(Token::Nil(_)) => Ok(Expr::Literal(Literal::Nil)),

            Some(Token::Number(_, lexeme)) => {
                let value = lexeme.parse().map_err(|e| errors::user_with_internal(
                    &format!("Unable to parse number '{}'.", lexeme),
                    "Make sure you have provided a valid number within the bounds of a 64-bit floating point number.",
                    e
                ))?;
                Ok(Expr::Literal(Literal::Number(value)))
            },
            Some(Token::String(_, lexeme)) => {
                let value = lexeme[1..lexeme.len() - 1].to_string();
                Ok(Expr::Literal(Literal::String(value)))
            },
            Some(Token::LeftParen(_)) => {
                let expr = Self::expression(context)?;
                rd_consume!(context, RightParen => Expr::Grouping(Box::new(expr)), "Expected a closing parenthesis `)` after the expression", "Make sure you have a closing parenthesis `)` after the expression.")
            },
            Some(var @ Token::Identifier(..)) => {
                Ok(Expr::Var(var))
            },
            Some(t) => {
                Self::synchronize(context);

                Err(errors::user(
                    &format!("Encountered an unexpected {:?} token while waiting for one of ['true', 'false', 'nil', number, string, '('].", t),
                    "Make sure that you are providing a primary value at this location.",
                ))
            },
            None => Err(errors::user(
                "Reached the end of the input while waiting for one of ['true', 'false', 'nil', number, string, '('].",
                "Make sure that you have provided a valid expression."))
        }
    });

    fn synchronize<T: Iterator<Item = Token>>(
        context: &mut ParseContext<T>
    ) {
        while let Some(token) = context.tokens.next() {
            match (token, context.tokens.peek()) {
                // If we reach a semicolon, we can stop because the next token will be the start of a new statement
                (Token::Semicolon(_), _) => break,
                // If the next token is the start of a new statement, we can stop
                (
                    _,
                    Some(
                        Token::Class(_)
                        | Token::Fun(_)
                        | Token::Var(_)
                        | Token::For(_)
                        | Token::If(_)
                        | Token::While(_)
                        | Token::Print(_)
                        | Token::Return(_),
                    ),
                ) => break,
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{printer::AstPrinter, StmtVisitor, ExprVisitor},
        lexer::Scanner, LoxError,
    };

    use super::Parser;

    fn test_parse_expr(source: &str, expected: &str) {
        let lexer = Scanner::new(source);
        let expr = Parser::parse_expr(&mut lexer.filter_map(|x| x.ok())).expect("no errors");
         assert_eq!(
            AstPrinter {}.visit_expr(&expr),
            expected,
            "the expression should be parsed correctly"
        );
    }

    fn test_parse(source: &str, expected: &str) {
        let lexer = Scanner::new(source);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));

        for err in errs {
            panic!("{}", err);
        }

        assert_eq!(
            AstPrinter {}.visit_stmt(tree.first().unwrap()),
            expected,
            "the expression should be parsed correctly"
        );
    }

    fn test_parse_err(source: &str) -> Vec<LoxError> {
        let lexer = Scanner::new(source);
        let (_, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        assert!(!errs.is_empty(), "errors should be returned");

        errs
    }

    #[test]
    fn parse_basic_expression() {
        test_parse_expr("1 + 2", "(+ 1 2)");
        test_parse_expr("10 - 5 / (2 * 3)", "(- 10 (/ 5 (group (* 2 3))))");
    }

    #[test]
    fn parse_block() {
        test_parse("{ 10; 20; 30; }", "(block (10) (20) (30))");
    }

    #[test]
    fn parse_var_def() {
        test_parse("var a = 10;", "(var a 10)");
    }

    #[test]
    fn parse_if() {
        test_parse("if (x > 5) { 10; } else { 20; }", "(if (> x 5) (block (10)) (block (20)))");
    }

    #[test]
    fn parse_while() {
        test_parse("while (x > 5) { 10; }", "(while (> x 5) (block (10)))");
    }

    #[test]
    fn parse_for() {
        test_parse("for (var i = 0; i < 10; i = i + 1) { 10; }", "(block (var i 0) (while (< i 10) (block (block (10)) ((= i (+ i 1))))))");
    }

    #[test]
    fn parse_break() {
        test_parse("while (true) { break; }", "(while true (block break))");
        test_parse_err("if (true) { break; }");
    }

    #[test]
    fn parse_function_call() {
        test_parse("clock();", "(call clock)");
        test_parse("f(1, 2, 3);", "(call f 1 2 3)");
    }

    #[test]
    fn parse_function_def() {
        test_parse("fun f() {}", "(fun f (block))");
        test_parse("fun f(x, y) { x + y; }", "(fun f x y (block ((+ x y))))");
        test_parse("fun f(x, y) { return x + y; }", "(fun f x y (block (return (+ x y))))");

        test_parse("var f = fun(a) {};", "(var f (fun @anonymous a (block)))");
    }

    #[test]
    fn parse_class_def() {
        test_parse("class A {}", "(class A)");
        test_parse("class A { f() {} }", "(class A (fun f (block)))");
        test_parse("class A { f() {} g() {} }", "(class A (fun f (block)) (fun g (block)))");
    }
}
