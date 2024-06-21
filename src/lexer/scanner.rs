use crate::{LoxError, errors, core::Loc};

use super::Token;

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

    fn location(&self) -> Loc {
        Loc::Line { line: self.line }
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

    fn read_token(&mut self) -> Option<Result<Token, LoxError>> {
        while let Some((loc, char)) = self.chars.next() {
            let last_line = self.last_newline;
            let location = self.location();

            match char {
                ' ' => continue,
                '\r' => continue,
                '\t' => continue,
                '\n' => {
                    self.line += 1;
                    self.last_newline = loc;
                },
                '(' => return Some(Ok(Token::LeftParen(location))),
                ')' => return Some(Ok(Token::RightParen(location))),
                '{' => return Some(Ok(Token::LeftBrace(location))),
                '}' => return Some(Ok(Token::RightBrace(location))),
                ',' => return Some(Ok(Token::Comma(location))),
                '.' => return Some(Ok(Token::Dot(location))),
                '-' => return Some(Ok(Token::Minus(location))),
                '+' => return Some(Ok(Token::Plus(location))),
                ';' => return Some(Ok(Token::Semicolon(location))),
                '*' => return Some(Ok(Token::Star(location))),

                '!' if self.match_char('=') => return Some(Ok(Token::BangEqual(location))),
                '!' => return Some(Ok(Token::Bang(location))),
                '=' if self.match_char('=') => return Some(Ok(Token::EqualEqual(location))),
                '=' => return Some(Ok(Token::Equal(location))),
                '>' if self.match_char('=') => return Some(Ok(Token::GreaterEqual(location))),
                '>' => return Some(Ok(Token::Greater(location))),
                '<' if self.match_char('=') => return Some(Ok(Token::LessEqual(location))),
                '<' => return Some(Ok(Token::Less(location))),

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
                '/' => return Some(Ok(Token::Slash(location))),

                '"' => return Some(self.read_string(loc)),

                c if c.is_numeric() => return Some(self.read_number(loc)),
                c if c.is_alphabetic() || c == '_' => return Some(self.read_identifier(loc)),

                c => return Some(Err(errors::language(
                    self.location().with_sample(self.source[last_line..loc].to_string()),
                    format!("We found an unexpected character '{}' where we were expecting one of: [whitespace, parenthesis, brace, operator, identifier, number, string, comment]", c),
                    "Make sure you have entered valid Lox code and have not accidentally closed a string.",
                )))
            }
        }

        None
    }

    fn read_string(&mut self, start: usize) -> Result<Token, LoxError> {
        let location = self.location();
        
        while let Some((loc, c)) = self.chars.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.last_newline = loc;
                },
                '"' => {
                    return Ok(Token::String(location, self.source[start..loc+1].to_string()));
                },
                _ => {}
            }
        }

        Err(errors::language(
            location.with_sample(self.source[start..start+20].to_string()),
            "Reached the end of the file without finding the closing quote for a string.",
            "Make sure that you have terminated your string with a '\"' character.",
        ))
    }

    fn read_number(&mut self, start: usize) -> Result<Token, LoxError> {
        let location = self.location();

        let mut end = start+self.advance_while_fn(|_, c| c.is_numeric());
        if let Some((loc, c)) = self.chars.peek() {
            if *c == '.' && self.source.chars().nth(loc + 1).map(|c2| c2.is_numeric()).unwrap_or_default() {
                self.chars.next();
                end += 1 + self.advance_while_fn(|_, c| c.is_numeric());
            }
        }

        Ok(Token::Number(location, self.source[start..end+1].to_string()))
    }

    fn read_identifier(&mut self, start: usize) -> Result<Token, LoxError> {
        let location = self.location();

        let end = start + self.advance_while_fn(|_, c| c.is_alphanumeric() || c == '_');
        let lexeme = &self.source[start..end+1];

        match lexeme {
            "and" => Ok(Token::And(location)),
            "break" => Ok(Token::Break(location)),
            "class" => Ok(Token::Class(location)),
            "else" => Ok(Token::Else(location)),
            "false" => Ok(Token::False(location)),
            "for" => Ok(Token::For(location)),
            "fun" => Ok(Token::Fun(location)),
            "if" => Ok(Token::If(location)),
            "nil" => Ok(Token::Nil(location)),
            "or" => Ok(Token::Or(location)),
            "print" => Ok(Token::Print(location)),
            "return" => Ok(Token::Return(location)),
            "super" => Ok(Token::Super(location)),
            "this" => Ok(Token::This(location)),
            "true" => Ok(Token::True(location)),
            "var" => Ok(Token::Var(location)),
            "while" => Ok(Token::While(location)),
            lexeme => Ok(Token::Identifier(location, lexeme.to_string())),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, LoxError>;

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

        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::Plus(Loc::new(1)));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::Minus(Loc::new(1)));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::Star(Loc::new(1)));
        assert_eq!(lexer.next().expect("a token").expect("without an error"), Token::Slash(Loc::new(1)));
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
            "(", "(", ")", ")", "{", "}",
            "!", "*", "+", "-", "/", "=", "<", ">", "<=", "==",
        ];

        for token in tokens {
            assert_eq!(lexer.next().expect("a token").expect("without an error").lexeme(), token);
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

        if let Token::String(_, lexeme) = lexer.next().expect("a token").expect("without an error") {
            assert_eq!(lexeme, "\"test\"");
        } else {
            panic!("expected a string token");
        }

        assert!(lexer.next().is_none(), "no more tokens");
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Scanner::new(r#" 123 12.34 12. "#);

        let numbers = ["123", "12.34", "12"];
        for number in numbers {
            if let Token::Number(_, lexeme) = lexer.next().expect("a token").expect("without an error") {
                assert_eq!(lexeme, number);
            } else {
                panic!("expected a number token");
            }
        }

        if let Token::Dot(_) = lexer.next().expect("a token").expect("without an error") {} else {
            panic!("expected a dot token");
        }

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
            if let Token::Identifier(_, lexeme) = lexer.next().expect("a token").expect("without an error") {
                assert_eq!(lexeme, identifier);
            } else {
                panic!("expected an identifier token");
            }
        }

        let keywords = ["and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while"];
        for keyword in keywords {
            let token = lexer.next().expect("a token").expect("without an error");
            assert_eq!(token.lexeme(), keyword);
        }
    }
}