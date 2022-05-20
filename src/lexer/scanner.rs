use crate::{LoxError, errors};

use super::{TokenType, Token};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    line: usize,
    last_newline: usize,
    has_err: bool,
}

#[allow(clippy::while_let_on_iterator)]
impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            line: 1,
            last_newline: 0,
            has_err: false,
        }
    }

    pub fn has_error(&self) -> bool {
        self.has_err
    }

    fn location(&self, loc: usize) -> (usize, usize) {
        let line = self.line;
        let column = loc - self.last_newline;
        (line, column)
    }

    fn err<F: FnOnce(usize, usize) -> LoxError>(&mut self, location: usize, f: F) -> LoxError {
        self.has_err = true;
        let (line, column) = self.location(location);
        f(line, column)
    }

    fn match_char(&mut self, next: char) -> bool {
        if let Some((_, c)) = self.chars.peek() {
            if *c == next {
                self.chars.next();
                return true
            }
        }

        false
    }

    fn advance_while_fn<F: Fn(usize, char) -> bool>(&mut self, f: F) -> usize {
        let mut length = 0;
        while let Some((loc, c)) = self.chars.peek() {
            if !f(*loc, *c) {
                break;
            }

            if *c == '\n' {
                self.line += 1;
                self.last_newline = *loc;
            }

            self.chars.next();
            length += 1;
        }

        length
    }

    fn read_token(&mut self) -> Option<Result<Token<'a>, LoxError>> {
        while let Some((loc, char)) = self.chars.next() {
            let (line, column) = self.location(loc);

            match char {
                ' ' => continue,
                '\r' => continue,
                '\t' => continue,
                '\n' => {
                    self.line += 1;
                    self.last_newline = loc;
                },
                '(' => return Some(Ok(Token::new(TokenType::LeftParen, "(", line, column))),
                ')' => return Some(Ok(Token::new(TokenType::RightParen, ")", line, column))),
                '{' => return Some(Ok(Token::new(TokenType::LeftBrace, "{", line, column))),
                '}' => return Some(Ok(Token::new(TokenType::RightBrace, "}", line, column))),
                ',' => return Some(Ok(Token::new(TokenType::Comma, ",", line, column))),
                '.' => return Some(Ok(Token::new(TokenType::Dot, ".", line, column))),
                '-' => return Some(Ok(Token::new(TokenType::Minus, "-", line, column))),
                '+' => return Some(Ok(Token::new(TokenType::Plus, "+", line, column))),
                ';' => return Some(Ok(Token::new(TokenType::Semicolon, ";", line, column))),
                '*' => return Some(Ok(Token::new(TokenType::Star, "*", line, column))),

                '!' if self.match_char('=') => return Some(Ok(Token::new(TokenType::BangEqual, "!=", line, column))),
                '!' => return Some(Ok(Token::new(TokenType::Bang, "!", line, column))),
                '=' if self.match_char('=') => return Some(Ok(Token::new(TokenType::EqualEqual, "==", line, column))),
                '=' => return Some(Ok(Token::new(TokenType::Equal, "=", line, column))),
                '>' if self.match_char('=') => return Some(Ok(Token::new(TokenType::GreaterEqual, ">=", line, column))),
                '>' => return Some(Ok(Token::new(TokenType::Greater, ">", line, column))),
                '<' if self.match_char('=') => return Some(Ok(Token::new(TokenType::LessEqual, "<=", line, column))),
                '<' => return Some(Ok(Token::new(TokenType::Less, "<", line, column))),

                '/' if self.match_char('/') => {
                    while let Some((_, c)) = self.chars.peek() {
                        if *c == '\n' {
                            self.chars.next();
                            self.line += 1;
                            self.last_newline = loc;
                            break;
                        } else {
                            self.chars.next();
                        }
                    }
                },
                '/' if self.match_char('*') => {
                    let mut depth = 1;
                    while let Some((_, c)) = self.chars.next() {
                        match c {
                            '\n' => {
                                self.line += 1;
                                self.last_newline = loc;
                            },
                            '/' if self.match_char('*') => {
                                depth += 1;
                            },
                            '*' if self.match_char('/') => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            },
                            _ => {}
                        }
                    }
                },
                '/' => return Some(Ok(Token::new(TokenType::Slash, "/", line, column))),

                '"' => return Some(self.read_string(loc)),

                c if c.is_numeric() => return Some(self.read_number(loc)),
                c if c.is_alphabetic() || c == '_' => return Some(self.read_identifier(loc)),

                c => return Some(Err(self.err(loc, |line, col| errors::user_with_internal(
                    &format!("We found an unexpected character '{}' where we were expecting one of: [whitespace, parenthesis, brace, operator, identifier, number, string, comment]", c),
                    "Make sure you have entered valid Lox code and have not accidentally closed a string.",
                    errors::source_location(self.source[loc - 5..loc+2].to_string(), line, col),
                ))))
            }
        }

        None
    }

    fn read_string(&mut self, start: usize) -> Result<Token<'a>, LoxError> {
        let (line, column) = self.location(start);
        
        while let Some((loc, c)) = self.chars.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.last_newline = loc;
                },
                '"' => {
                    return Ok(Token::new(TokenType::String, &self.source[start..loc+1], line, column));
                },
                _ => {}
            }
        }

        Err(self.err(start, |line, col| errors::user_with_internal(
            "Reached the end of the file without finding the closing quote for a string.",
            "Make sure that you have terminated your string with a '\"' character.",
            errors::source_location(self.source[start..start+10].to_string(), line, col),
        )))
    }

    fn read_number(&mut self, start: usize) -> Result<Token<'a>, LoxError> {
        let (line, column) = self.location(start);

        let mut end = start+self.advance_while_fn(|_, c| c.is_numeric());
        if let Some((loc, c)) = self.chars.peek() {
            if *c == '.' && self.source.chars().nth(loc + 1).map(|c2| c2.is_numeric()).unwrap_or_default() {
                self.chars.next();
                end += 1 + self.advance_while_fn(|_, c| c.is_numeric());
            }
        }

        return Ok(Token::new(TokenType::Number, &self.source[start..end+1], line, column));
    }

    fn read_identifier(&mut self, start: usize) -> Result<Token<'a>, LoxError> {
        let (line, column) = self.location(start);

        let end = start + self.advance_while_fn(|_, c| c.is_alphanumeric() || c == '_');
        let lexeme = &self.source[start..end+1];

        match lexeme {
            "and" => Ok(Token::new(TokenType::And, lexeme, line, column)),
            "class" => Ok(Token::new(TokenType::Class, lexeme, line, column)),
            "else" => Ok(Token::new(TokenType::Else, lexeme, line, column)),
            "false" => Ok(Token::new(TokenType::False, lexeme, line, column)),
            "for" => Ok(Token::new(TokenType::For, lexeme, line, column)),
            "fun" => Ok(Token::new(TokenType::Fun, lexeme, line, column)),
            "if" => Ok(Token::new(TokenType::If, lexeme, line, column)),
            "nil" => Ok(Token::new(TokenType::Nil, lexeme, line, column)),
            "or" => Ok(Token::new(TokenType::Or, lexeme, line, column)),
            "print" => Ok(Token::new(TokenType::Print, lexeme, line, column)),
            "return" => Ok(Token::new(TokenType::Return, lexeme, line, column)),
            "super" => Ok(Token::new(TokenType::Super, lexeme, line, column)),
            "this" => Ok(Token::new(TokenType::This, lexeme, line, column)),
            "true" => Ok(Token::new(TokenType::True, lexeme, line, column)),
            "var" => Ok(Token::new(TokenType::Var, lexeme, line, column)),
            "while" => Ok(Token::new(TokenType::While, lexeme, line, column)),
            lexeme => Ok(Token::new(TokenType::Identifier, lexeme, line, column)),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, LoxError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operators() {
        let mut lexer = Scanner::new("+ - * /");

        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::new(TokenType::Plus, "+", 1, 0));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::new(TokenType::Minus, "-", 1, 2));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::new(TokenType::Star, "*", 1, 4));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::new(TokenType::Slash, "/", 1, 6));
        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_basic_symbols() {
        let mut lexer = Scanner::new(r#"
// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators
"#);
        
        let tokens = [
            TokenType::LeftParen, TokenType::LeftParen, TokenType::RightParen, TokenType::RightParen, TokenType::LeftBrace, TokenType::RightBrace,
            TokenType::Bang, TokenType::Star, TokenType::Plus, TokenType::Minus, TokenType::Slash, TokenType::Equal, TokenType::Less, TokenType::Greater, TokenType::LessEqual, TokenType::EqualEqual
        ];

        for token in tokens {
            assert_eq!(lexer.next().expect("a token").expect("without an error").token_type(), token);
        }

        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_comments() {
        let mut lexer = Scanner::new(r#"
// single line comment
/* multi-line comment on a single line */
/*
* multi-line comment
* on multiple lines
*/
/*/* Nested multi-line comment! */*/
        "#);

        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_strings() {
        let mut lexer = Scanner::new(r#" "test" "#);

        let token = lexer.next().expect("a token").expect("without an error");
        assert_eq!(token.token_type(), TokenType::String);
        assert_eq!(token.lexeme(), "\"test\"");

        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Scanner::new(r#" 123 12.34 12. "#);

        let numbers = ["123", "12.34", "12"];
        for number in numbers {
            let token = lexer.next().expect("a token").expect("without an error");
            assert_eq!(token.token_type(), TokenType::Number);
            assert_eq!(token.lexeme(), number);
        }

        assert_eq!(lexer.next().expect("a token").expect("without an error").token_type(), TokenType::Dot);

        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let mut lexer = Scanner::new(r#"
identifier _id a_b_c
and class else false for fun if nil or print return super this true var while
"#);

        let identifiers = ["identifier", "_id", "a_b_c"];
        for identifier in identifiers {
            let token = lexer.next().expect("a token").expect("without an error");
            assert_eq!(token.token_type(), TokenType::Identifier);
            assert_eq!(token.lexeme(), identifier);
        }

        let keywords = [TokenType::And, TokenType::Class, TokenType::Else, TokenType::False, TokenType::For, TokenType::Fun, TokenType::If, TokenType::Nil, TokenType::Or, TokenType::Print, TokenType::Return, TokenType::Super, TokenType::This, TokenType::True, TokenType::Var, TokenType::While];
        for keyword in keywords {
            let token = lexer.next().expect("a token").expect("without an error");
            assert_eq!(token.token_type(), keyword);
        }
    }
}