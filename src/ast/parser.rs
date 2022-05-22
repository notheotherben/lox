use std::iter::Peekable;

use crate::{errors, lexer::Token, LoxError};

use super::{Expr, Literal, Stmt};

#[derive(Default, Debug)]
pub struct Parser {
    loop_depth: usize,
}

// Macros which make it easier to implement certain common parts of the parser.
macro_rules! rd_term {
    ($name:ident($self:ident) := $token_id:ident => Vec<Stmt> : $body:expr) => {
        rd_term!($name($self) := $token_id => Vec<Stmt<'a>> : $body);
    };

    ($name:ident($self:ident) := $token_id:ident => Stmt : $body:expr) => {
        rd_term!($name($self) := $token_id => Stmt<'a> : $body);
    };

    ($name:ident($self:ident) := $token_id:ident => Expr : $body:expr) => {
        rd_term!($name($self) := $token_id => Expr<'a> : $body);
    };

    ($name:ident($self:ident) := $token_id:ident => $ret:ty : $body:expr) => {
        fn $name<'a, T: Iterator<Item = Token<'a>>>(
            &mut $self,
            $token_id: &mut Peekable<T>,
        ) -> Result<$ret, LoxError> {
            $body
        }
    };

    ($name:ident($self:ident) := $left:ident ( $($token:ident)|+ $right:ident )* => binary) => {
        rd_term!($name($self) := tokens => Expr : {
            let left = $self.$left(tokens)?;
    
            if !matches!(tokens.peek(), Some($(Token::$token(..))|+)) {
                return Ok(left)
            }
    
            let op = tokens.next().unwrap();
            let right = $self.$right(tokens)?;
            Ok(Expr::Binary(
                Box::new(left),
                op,
                Box::new(right),
            ))
        });
    };

    ($name:ident($self:ident) := ($($token:ident)|+ $right:ident) | $fallback:ident => unary) => {
        rd_term!($name($self) := tokens => Expr : {
            if !matches!(tokens.peek(), Some($(Token::$token(_))|+)) {
                return $self.$fallback(tokens);
            };
    
            let op = tokens.next().unwrap();
            let right = $self.$right(tokens)?;
            Ok(Expr::Unary(op, Box::new(right)))
        });
    };
}

macro_rules! rd_matches {
    ($tokens:ident, $($token:ident)|+) => {
        if matches!($tokens.peek(), Some($(Token::$token(..))|+)) {
            Some($tokens.next().unwrap())
        } else {
            None
        }
    };
}

macro_rules! rd_consume {
    ($tokens:ident, $($id:ident@$token:ident)|+ => $ok:expr, $msg:expr, $advice:expr) => {
        match $tokens.next() {
            Some($($id@Token::$token(..))|+) => $ok,
            Some(close_paren) => return Err(errors::user(
                &format!("{}, but got {} instead.", $msg, close_paren),
                $advice
            )),
            None => return Err(errors::user(
                &format!("{}, but reached the end of the file instead.", $msg),
                $advice
            )),
        }
    };

    ($tokens:ident, $($token:ident)|+ => $ok:expr, $msg:expr, $advice:expr) => {
        match $tokens.next() {
            Some($(Token::$token(..))|+) => $ok,
            Some(close_paren) => return Err(errors::user(
                &format!("{}, but got {} instead.", $msg, close_paren),
                $advice
            )),
            None => return Err(errors::user(
                &format!("{}, but reached the end of the file instead.", $msg),
                $advice
            )),
        }
    };

    ($tokens:ident, $($token:ident)|+, $msg:expr, $advice:expr) => {
        rd_consume!($tokens, $($token)|+ => {}, $msg, $advice)
    };
}

impl Parser {
    pub fn parse<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut T,
    ) -> (Vec<Stmt<'a>>, Vec<LoxError>) {
        let mut parser = Self::default();
        let mut tokens = tokens.peekable();
        let mut stmts = Vec::new();
        let mut errs = Vec::new();

        while tokens.peek().is_some() {
            match parser.declaration(&mut tokens) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    parser.synchronize(&mut tokens);
                    errs.push(err);
                },
            }
        }

        (stmts, errs)
    }

    pub fn parse_expr<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut T,
    ) -> Result<Expr<'a>, LoxError> {
        let mut parser = Self::default();
        let mut tokens = tokens.peekable();
        parser.expression(&mut tokens)
    }

    rd_term!(declaration(self) := tokens => Stmt : {
        if rd_matches!(tokens, Var).is_none() {
            return self.statement(tokens);
        }

        rd_consume!(tokens, ident@Identifier => {
            let init = if let Some(Token::Equal(_)) = tokens.peek() {
                tokens.next();
                self.expression(tokens)?
            } else {
                Expr::Literal(Literal::Nil)
            };

            rd_consume!(
                tokens,
                Semicolon => Ok(Stmt::Var(ident, init)),
                "Expected ';' after variable declaration",
                "Make sure that you have a semicolon after the variable declaration.")
            },
            "Expected an identifier to be provided after 'var'",
            "Provide a variable name after the `var` keyword.")
    });

    rd_term!(statement(self) := tokens => Stmt : {
        let stmt = match tokens.peek() {
            Some(Token::For(_)) => {
                tokens.next();
                self.loop_depth += 1;
                let stmt = self.for_statement(tokens);
                self.loop_depth -= 1;
                return stmt
            },
            Some(Token::If(_)) => {
                tokens.next();
                return self.if_statement(tokens)
            },
            Some(Token::LeftBrace(_)) => {
                tokens.next();
                return Ok(Stmt::Block(self.block(tokens)?))
            },
            Some(Token::While(_)) => {
                tokens.next();
                self.loop_depth += 1;
                let stmt = self.while_statement(tokens);
                self.loop_depth -= 1;
                return stmt
            },
            Some(Token::Break(_)) => {
                tokens.next();

                if self.loop_depth == 0 {
                    return Err(errors::user(
                        "Cannot use 'break' outside of a loop.",
                        "Make sure that you are within the bounds of a `while` or `for` loop when using the `break` keyword."
                    ));
                }

                Stmt::Break
            },
            Some(Token::Print(_)) => {
                tokens.next();
                let expr = self.expression(tokens)?;
                Stmt::Print(expr)
            },
            _ => {
                let expr = self.expression(tokens)?;
                Stmt::Expression(expr)
            }
        };

        rd_consume!(tokens, Semicolon => Ok(stmt), "Expected ';' after expression", "Make sure that you have a semicolon at the end of your previous expression.")
    });

    rd_term!(if_statement(self) := tokens => Stmt : {
        rd_consume!(tokens, LeftParen, "Expected an opening parenthesis `(` after the `if` keyword", "Make sure you have an opening parenthesis `(` after the `if` keyword.");

        let condition = self.expression(tokens)?;

        rd_consume!(tokens, RightParen, "Expected a closing parenthesis `)` after the `if` keyword's condition", "Make sure you have a closing parenthesis `)` after the `if` keyword's condition.");
        
        let then_branch = self.statement(tokens)?;

        Ok(Stmt::If(
            condition,
            Box::new(then_branch),
            if rd_matches!(tokens, Else).is_some() {
                let else_branch = self.statement(tokens)?;
                 Some(Box::new(else_branch))
            } else {
                None
            }
        ))
    });

    rd_term!(for_statement(self) := tokens => Stmt : {
        rd_consume!(tokens, LeftParen, "Expected an opening parenthesis `(` after the `for` keyword", "Make sure you have an opening parenthesis `(` after the `for` keyword.");

        let init = match tokens.peek() {
            Some(Token::Semicolon(_)) => {
                tokens.next();
                None
            },
            Some(Token::Var(_)) => {
                Some(self.declaration(tokens)?)
            },
            _ => {
                let expr = Stmt::Expression(self.expression(tokens)?);
                rd_consume!(tokens, Semicolon, "Expected a semicolon after the initializer", "Make sure you have a semicolon after the initializer.");
                Some(expr)
            }
        };

        let cond = if rd_matches!(tokens, Semicolon).is_some() {
            None
        } else {
            let cond = self.expression(tokens)?;
            rd_consume!(tokens, Semicolon, "Expected a semicolon after the condition", "Make sure you have a semicolon after the condition.");
            Some(cond)
        };

        let incr = if rd_matches!(tokens, RightParen).is_some() {
            None
        } else {
            let incr = self.expression(tokens)?;
            rd_consume!(tokens, RightParen, "Expected a closing parenthesis `)` after the `for` keyword's condition", "Make sure you have a closing parenthesis `)` after the `for` keyword's condition.");
            Some(incr)
        };

        let mut body = self.statement(tokens)?;

        if let Some(incr) = incr {
            body = Stmt::Block(vec![body, Stmt::Expression(incr)]);
        }

        body = Stmt::While(cond.unwrap_or(Expr::Literal(Literal::Bool(true))), Box::new(body));

        if let Some(init) = init {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    });

    rd_term!(while_statement(self) := tokens => Stmt : {
        rd_consume!(tokens, LeftParen, "Expected an opening parenthesis `(` after the `while` keyword", "Make sure you have an opening parenthesis `(` after the `while` keyword.");

        let condition = self.expression(tokens)?;

        rd_consume!(tokens, RightParen, "Expected a closing parenthesis `)` after the `while` keyword's condition", "Make sure you have a closing parenthesis `)` after the `while` keyword's condition.");
        

        let body = self.statement(tokens)?;

        Ok(Stmt::While(condition, Box::new(body)))
    });

    rd_term!(block(self) := tokens => Vec<Stmt> : {
        let mut stmts = Vec::new();

        while !matches!(tokens.peek(), Some(Token::RightBrace(_)) | None) {
            match self.declaration(tokens) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.synchronize(tokens);
                    return Err(err)
                },
            }
        }

        rd_consume!(tokens, RightBrace => Ok(stmts), "Expected a closing brace `}` after the block", "Make sure you have a closing brace `}` after the block.")
    });

    rd_term!(expression(self) := tokens => Expr : self.assignment(tokens));

    rd_term!(assignment(self) := tokens => Expr : {
        let expr = self.or(tokens)?;

        if let Some(equals) = rd_matches!(tokens, Equal) {
            let value = self.assignment(tokens)?;

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

    rd_term!(or(self) := and (Or and)* => binary);

    rd_term!(and(self) := equality (And equality)* => binary);

    rd_term!(equality(self) := comparison (BangEqual|EqualEqual equality)* => binary);

    rd_term!(comparison(self) := term (Greater | GreaterEqual | Less | LessEqual equality)* => binary);

    rd_term!(term(self) := factor (Minus | Plus equality)* => binary);

    rd_term!(factor(self) := unary (Star | Slash equality)* => binary);

    rd_term!(unary(self) := (Bang|Minus term)| primary => unary);

    rd_term!(primary(self) := tokens => Expr : {
        match tokens.next() {
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
                let expr = self.expression(tokens)?;
                rd_consume!(tokens, RightParen => Ok(Expr::Grouping(Box::new(expr))), "Expected a closing parenthesis `)` after the expression", "Make sure you have a closing parenthesis `)` after the expression.")
            },
            Some(var @ Token::Identifier(..)) => {
                Ok(Expr::Var(var))
            },
            Some(t) => {
                self.synchronize(tokens);

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

    fn synchronize<'a, T: Iterator<Item = Token<'a>>>(
        &mut self,
        tokens: &mut Peekable<T>
    ) {
        while let Some(token) = tokens.next() {
            match (token, tokens.peek()) {
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
            AstPrinter {}.visit_expr(expr),
            expected,
            "the expression should be parsed correctly"
        );
    }

    fn test_parse(source: &str, expected: &str) {
        let lexer = Scanner::new(source);
        let (tree, errs) = Parser::parse(&mut lexer.filter_map(|x| x.ok()));
        assert!(errs.is_empty(), "no errors should be returned");

        assert_eq!(
            AstPrinter {}.visit_stmt(tree.first().unwrap().clone()),
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
}
