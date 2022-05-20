use std::iter::Peekable;

use crate::{errors, lexer::Token, LoxError};

use super::{Expr, Literal, Stmt};

pub struct Parser;

impl Parser {
    pub fn parse<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut T,
    ) -> Result<Vec<Stmt<'a>>, LoxError> {
        let mut tokens = tokens.peekable();
        let mut stmts = Vec::new();
        while tokens.peek().is_some() {
            match Self::declaration(&mut tokens) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    Self::synchronize(&mut tokens);
                    // TODO: This should be reported as its own iterable
                    return Err(err)
                },
            }
        }

        Ok(stmts)
    }

    pub fn parse_expr<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut T,
    ) -> Result<Expr<'a>, LoxError> {
        let mut tokens = tokens.peekable();
        Self::expression(&mut tokens)
    }


    fn declaration<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Stmt<'a>, LoxError> {
        match tokens.peek() {
            Some(Token::Var(_)) => {},
            _ => return Self::statement(tokens),
        };

        let var = tokens.next().unwrap();
        match tokens.next() {
            Some(ident @ Token::Identifier(..)) => {
                let init = if let Some(Token::Equal(_)) = tokens.peek() {
                    tokens.next();
                    Self::expression(tokens)?
                } else {
                    Expr::Literal(Literal::Nil)
                };

                match tokens.next() {
                    Some(Token::Semicolon(_)) => Ok(Stmt::Var(ident, init)),
                    Some(tok) => Err(errors::user(
                        &format!("Expected `;` after variable declaration {}, found `{}`", var, tok),
                        "Make sure that you have a semicolon after the variable declaration."
                    )),
                    None => Err(errors::user(
                        &format!("Expected `;` after variable declaration {}, found end of input.", var),
                        "Make sure that you have a semicolon after the variable declaration."
                    )),
                }
            },
            Some(other) => Err(errors::user(
                &format!("Expected an identifier to be provided after {}, but got {} instead.", var, other),
                "Provide a variable name after the `var` keyword."
            )),
            None => Err(errors::user(
                &format!("Expected an identifier to be provided after {}, but we found the end of the file instead.", var),
                "Provide a variable name after the `var` keyword."
            )),
        }
    }

    fn statement<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Stmt<'a>, LoxError> {
        let stmt = match tokens.peek() {
            Some(Token::Print(_)) => {
                tokens.next();
                let expr = Self::expression(tokens)?;
                Stmt::Print(expr)
            }
            _ => {
                let expr = Self::expression(tokens)?;
                Stmt::Expression(expr)
            }
        };

        match tokens.next() {
            Some(Token::Semicolon(_)) => Ok(stmt),
            Some(right) => {
                Self::synchronize(tokens);
                Err(errors::user(
                    &format!("Expected a semicolon to end the expression, but got a {} instead.", right),
                    "Make sure you have provided a terminating semicolon at the end of your previous expression."))
            },
            None => {
                Err(errors::user(
                    "Expected a semicolon to end the expression, but reached the end of the file instead.",
                    "Make sure you have provided a terminating semicolon at the end of your previous expression."))
            }
        }
    }

    fn expression<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        Self::assignment(tokens)
    }

    fn assignment<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        let expr = Self::equality(tokens)?;

        match tokens.peek() {
            Some(Token::Equal(_)) => {},
            _ => return Ok(expr),
        };

        let equals = tokens.next().unwrap();
        let value = Self::assignment(tokens)?;

        match expr {
            Expr::Var(name) => Ok(Expr::Assign(name, Box::new(value))),
            _ => Err(errors::user(
                &format!("Expected a variable identifier to be assigned to, but got {:?} instead at {}.", expr, equals),
                "Make sure that you provide the name of a variable to assign to."
            )),
        }
    }

    fn equality<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        let left = Self::comparison(tokens)?;

        match tokens.peek() {
            Some(Token::BangEqual(_) | Token::EqualEqual(_)) => {}
            _ => return Ok(left),
        }

        let op = tokens.next().unwrap();
        let right = Self::equality(tokens)?;
        Ok(Expr::Binary(
            Box::new(left),
            op,
            Box::new(right),
        ))
    }

    fn comparison<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        let left = Self::term(tokens)?;

        match tokens.peek() {
            Some(
                Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_),
            ) => {}
            _ => return Ok(left),
        }

        let op = tokens.next().unwrap();
        let right = Self::equality(tokens)?;
        Ok(Expr::Binary(
            Box::new(left),
            op,
            Box::new(right),
        ))
    }

    fn term<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        let left = Self::factor(tokens)?;

        match tokens.peek() {
            Some(
                Token::Minus(_) | Token::Plus(_),
            ) => {}
            _ => return Ok(left),
        }

        let op = tokens.next().unwrap();
        let right = Self::equality(tokens)?;
        Ok(Expr::Binary(
            Box::new(left),
            op,
            Box::new(right),
        ))
    }

    fn factor<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        let left = Self::unary(tokens)?;

        match tokens.peek() {
            Some(
                Token::Slash(_) | Token::Star(_),
            ) => {}
            _ => return Ok(left),
        }

        let op = tokens.next().unwrap();
        let right = Self::equality(tokens)?;
        Ok(Expr::Binary(
            Box::new(left),
            op,
            Box::new(right),
        ))
    }

    fn unary<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
        match tokens.peek() {
            Some(Token::Bang(_) | Token::Minus(_)) => {}
            _ => return Self::primary(tokens),
        };

        let op = tokens.next().unwrap();
        let right = Self::term(tokens)?;
        Ok(Expr::Unary(op, Box::new(right)))
    }

    fn primary<'a, T: Iterator<Item = Token<'a>>>(
        tokens: &mut Peekable<T>,
    ) -> Result<Expr<'a>, LoxError> {
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
                let expr = Self::expression(tokens)?;
                match tokens.next() {
                    Some(Token::RightParen(_)) => Ok(Expr::Grouping(Box::new(expr))),
                    Some(right) => {
                        Self::synchronize(tokens);
                        Err(errors::user(
                            &format!("Expected a closing parenthesis after the expression, but got a {} instead.", right),
                            "Make sure you have provided a closing parenthesis at the end of your expression."))
                    },
                    None => {
                        Err(errors::user(
                            "Expected a closing parenthesis after the expression, but reached the end of the file instead.",
                            "Make sure you have provided a closing parenthesis at the end of your expression."))
                    }
                }
            },
            Some(var @ Token::Identifier(..)) => {
                Ok(Expr::Var(var))
            },
            Some(t) => {
                Self::synchronize(tokens);

                Err(errors::user(
                    &format!("Encountered an unexpected {:?} token while waiting for one of ['true', 'false', 'nil', number, string, '('].", t),
                    "Make sure that you are providing a primary value at this location.",
                ))
            },
            None => Err(errors::user(
                "Reached the end of the input while waiting for one of ['true', 'false', 'nil', number, string, '('].",
                "Make sure that you have provided a valid expression."))
        }
    }

    fn synchronize<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut Peekable<T>) {
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
        ast::{printer::AstPrinter, ExprVisitor},
        lexer::Scanner,
    };

    use super::Parser;

    #[test]
    fn parse_basic_expression() {
        let lexer = Scanner::new("10 - 5 / (2 * 3)");
        let tree = Parser::parse_expr(&mut lexer.filter_map(|x| x.ok())).expect("no errors");

        assert_eq!(
            AstPrinter {}.visit_expr(tree),
            "(- 10 (/ 5 (group (* 2 3))))",
            "the expression should be parsed correctly"
        )
    }
}
