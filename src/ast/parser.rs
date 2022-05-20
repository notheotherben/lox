use std::iter::Peekable;

use crate::{lexer::{Token, TokenType}, LoxError, errors};

use super::{Expr, Literal, Stmt};

pub struct Parser;

impl Parser {
    pub fn parse<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut T) -> Result<Vec<Stmt<'a>>, LoxError> {
        let mut tokens = TokenIter::new(tokens);
        let mut stmts = Vec::new();
        while tokens.peek().is_some() {
            let stmt = Self::statement(&mut tokens)?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }

    pub fn parse_expr<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut T) -> Result<Expr<'a>, LoxError> {
        let mut tokens = TokenIter::new(tokens);
        Self::expression(&mut tokens)
    }

    fn statement<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Stmt<'a>, LoxError> {
        let stmt = if tokens.next_of(&[TokenType::Print]).is_some() {
            let expr = Self::expression(tokens)?;
            Stmt::Print(expr)
        } else {
            let expr = Self::expression(tokens)?;
            Stmt::Expression(expr)
        };

        if let Some(right) = tokens.next() {
            if right.is(TokenType::Semicolon) {
                return Ok(stmt);
            }

            Self::synchronize(tokens);
    
            Err(errors::user_with_internal(
                &format!("Expected a semicolon to end the expression, but got a {} instead.", right),
                "Make sure you have provided a terminating semicolon at the end of your previous expression.",
                right.location()))
        } else {
            Err(errors::user(
                "Expected a semicolon to end the expression, but got the end of the file instead.", 
                "Make sure you have ended your last expression with a semicolon."))
        }
    }

    fn expression<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        Self::equality(tokens)
    }

    fn equality<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        let mut left = Self::comparison(tokens)?;

        while let Some(op) = tokens.next_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let right = Self::comparison(tokens)?;
            left = Expr::Binary(Box::new(left), op.clone(), Box::new(right));
        }

        Ok(left)
    }

    fn comparison<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
       let mut left = Self::term(tokens)?;

       while let Some(op) = tokens.next_of(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
           let right = Self::term(tokens)?;
           left = Expr::Binary(Box::new(left), op.clone(), Box::new(right));
       }

       Ok(left)
    }

    fn term<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        let mut left = Self::factor(tokens)?;

        while let Some(op) = tokens.next_of(&[TokenType::Minus, TokenType::Plus]) {
            let right = Self::factor(tokens)?;
            left = Expr::Binary(Box::new(left), op.clone(), Box::new(right));
        }
 
        Ok(left)
    }

    fn factor<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        let mut left = Self::unary(tokens)?;

        while let Some(op) = tokens.next_of(&[TokenType::Slash, TokenType::Star]) {
            let right = Self::unary(tokens)?;
            left = Expr::Binary(Box::new(left), op.clone(), Box::new(right));
        }
 
        Ok(left)
    }

    fn unary<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        if let Some(op) = tokens.next_of(&[TokenType::Bang, TokenType::Minus]) {
            let right = Self::unary(tokens)?;
            Ok(Expr::Unary(op.clone(), Box::new(right)))
        } else {
            Self::primary(tokens)
        }
    }

    fn primary<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) -> Result<Expr<'a>, LoxError> {
        if let Some(token) = tokens.next() {
            match token.token_type() {
                TokenType::False => Ok(Expr::Literal(token, Literal::False)),
                TokenType::True => Ok(Expr::Literal(token, Literal::True)),
                TokenType::Nil => Ok(Expr::Literal(token, Literal::Nil)),

                TokenType::Number => {
                    let value = token.lexeme().parse().map_err(|e| errors::user_with_internal(
                        &format!("Unable to parse number '{}'.", token.lexeme()),
                        "Make sure you have provided a valid number within the bounds of a 64-bit floating point number.",
                        e
                    ))?;
                    Ok(Expr::Literal(token, Literal::Number(value)))
                },
                TokenType::String => {
                    let value = token.lexeme()[1..token.lexeme().len()-1].to_string();
                    Ok(Expr::Literal(token, Literal::String(value)))
                },

                TokenType::LeftParen => {
                    let expr = Self::expression(tokens)?;
                    if let Some(right) = tokens.next() {
                        if right.is(TokenType::RightParen) {
                            return Ok(Expr::Grouping(Box::new(expr)));
                        }
                    }

                    Self::synchronize(tokens);

                    Err(errors::user_with_internal(
                        "Could not find the end of an expression group where it was expected.",
                        "Make sure you have closed all open expression groups with a corresponding ')'.",
                        token.location()))
                },

                t => {
                    Self::synchronize(tokens);

                    Err(errors::user_with_internal(
                        &format!("Encountered an unexpected {:?} token while waiting for one of ['true', 'false', 'nil', number, string, '('].", t),
                        "Make sure that you are providing a primary value at this location.",
                        token.location(),
                    ))
                }
            }
        } else {
            Self::synchronize(tokens);

            Err(errors::user(
                "Reached the end of the input while waiting for one of ['true', 'false', 'nil', number, string, '('].",
                "Make sure that you have provided a valid expression."))
        }
    }

    fn synchronize<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut TokenIter<'a, T>) {
        while let Some(token) = tokens.next() {
            match token.token_type() {
                // If we reach a semicolon, we can stop because the next token will be the start of a new statement
                TokenType::Semicolon => break,
                // If the next token is the start of a new statement, we can stop
                _ if tokens.peek().map(|t| t.is_one_of(&[TokenType::Class, TokenType::Fun, TokenType::Var, TokenType::For, TokenType::If, TokenType::While, TokenType::Print, TokenType::Return])).unwrap_or_default() => break,
                _ => {}
            }
        }
    }
}

struct TokenIter<'a, I : Iterator<Item = Token<'a>>> {
    source: Peekable<I>
}

impl<'a, I: Iterator<Item = Token<'a>>> TokenIter<'a, I> {
    pub fn new(source: I) -> Self {
        Self {
            source: source.peekable()
        }
    }

    pub fn peek(&mut self) -> Option<&Token<'a>> {
        self.source.peek()
    }

    pub fn next_of(&mut self, types: &[TokenType]) -> Option<Token<'a>> {
        if let Some(token) = self.source.peek() {
            if token.is_one_of(types) {
                return self.source.next()
            }
        }

        None
    }
}

impl<'a, I: Iterator<Item = Token<'a>>> Iterator for TokenIter<'a, I> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.source.next()
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Scanner, ast::{printer::AstPrinter, ExprVisitor}};

    use super::Parser;

    #[test]
    fn parse_basic_expression()
    {
        let lexer = Scanner::new("10 - 5 / (2 * 3)");
        let tree = Parser::parse_expr(&mut lexer.filter_map(|x| x.ok())).expect("no errors");

        assert_eq!(AstPrinter{}.visit_expr(tree), "(- 10 (/ 5 (group (* 2 3))))", "the expression should be parsed correctly")
    }
}