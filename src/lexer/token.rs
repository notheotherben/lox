use std::fmt::Display;

use crate::core::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LeftParen(Location),
    RightParen(Location),
    LeftBrace(Location),
    RightBrace(Location),
    Comma(Location),
    Dot(Location),
    Minus(Location),
    Plus(Location),
    Semicolon(Location),
    Slash(Location),
    Star(Location),

    Bang(Location),
    BangEqual(Location),
    Equal(Location),
    EqualEqual(Location),
    Greater(Location),
    GreaterEqual(Location),
    Less(Location),
    LessEqual(Location),

    Identifier(Location, String),
    String(Location, String),
    Number(Location, String),

    And(Location),
    Break(Location),
    Class(Location),
    Else(Location),
    False(Location),
    Fun(Location),
    For(Location),
    If(Location),
    Nil(Location),
    Or(Location),
    Print(Location),
    Return(Location),
    Super(Location),
    This(Location),
    True(Location),
    Var(Location),
    While(Location),
}

impl Token {
    pub fn location(&self) -> Location {
        match self {
            Token::LeftParen(loc) => *loc,
            Token::RightParen(loc) => *loc,
            Token::LeftBrace(loc) => *loc,
            Token::RightBrace(loc) => *loc,
            Token::Comma(loc) => *loc,
            Token::Dot(loc) => *loc,
            Token::Minus(loc) => *loc,
            Token::Plus(loc) => *loc,
            Token::Semicolon(loc) => *loc,
            Token::Slash(loc) => *loc,
            Token::Star(loc) => *loc,

            Token::Bang(loc) => *loc,
            Token::BangEqual(loc) => *loc,
            Token::Equal(loc) => *loc,
            Token::EqualEqual(loc) => *loc,
            Token::Greater(loc) => *loc,
            Token::GreaterEqual(loc) => *loc,
            Token::Less(loc) => *loc,
            Token::LessEqual(loc) => *loc,

            Token::Identifier(loc, _) => *loc,
            Token::String(loc, _) => *loc,
            Token::Number(loc, _) => *loc,

            Token::And(loc) => *loc,
            Token::Break(loc) => *loc,
            Token::Class(loc) => *loc,
            Token::Else(loc) => *loc,
            Token::False(loc) => *loc,
            Token::Fun(loc) => *loc,
            Token::For(loc) => *loc,
            Token::If(loc) => *loc,
            Token::Nil(loc) => *loc,
            Token::Or(loc) => *loc,
            Token::Print(loc) => *loc,
            Token::Return(loc) => *loc,
            Token::Super(loc) => *loc,
            Token::This(loc) => *loc,
            Token::True(loc) => *loc,
            Token::Var(loc) => *loc,
            Token::While(loc) => *loc,
        }
    }

    pub fn lexeme(&self) -> &str {
        match self {
            Token::LeftParen(..) => "(",
            Token::RightParen(..) => ")",
            Token::LeftBrace(..) => "{",
            Token::RightBrace(..) => "}",
            Token::Comma(..) => ",",
            Token::Dot(..) => ".",
            Token::Minus(..) => "-",
            Token::Plus(..) => "+",
            Token::Semicolon(..) => ";",
            Token::Slash(..) => "/",
            Token::Star(..) => "*",

            Token::Bang(..) => "!",
            Token::BangEqual(..) => "!=",
            Token::Equal(..) => "=",
            Token::EqualEqual(..) => "==",
            Token::Greater(..) => ">",
            Token::GreaterEqual(..) => ">=",
            Token::Less(..) => "<",
            Token::LessEqual(..) => "<=",

            Token::Identifier(_, l) => l,
            Token::String(_, l) => l,
            Token::Number(_, l) => l,

            Token::And(..) => "and",
            Token::Break(..) => "break",
            Token::Class(..) => "class",
            Token::Else(..) => "else",
            Token::False(..) => "false",
            Token::Fun(..) => "fun",
            Token::For(..) => "for",
            Token::If(..) => "if",
            Token::Nil(..) => "nil",
            Token::Or(..) => "or",
            Token::Print(..) => "print",
            Token::Return(..) => "return",
            Token::Super(..) => "super",
            Token::This(..) => "this",
            Token::True(..) => "true",
            Token::Var(..) => "var",
            Token::While(..) => "while",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'{}' at {}", self.lexeme(), self.location())
    }
}

#[cfg(test)]
mod tests {
    use crate::core::Location;

    use super::Token;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Token::If(Location::new(1, 1))), "'if' at line 1, column 1");
    }
}