use std::fmt::Display;

use crate::core::Loc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    LeftParen(Loc),
    RightParen(Loc),
    LeftBrace(Loc),
    RightBrace(Loc),
    Comma(Loc),
    Dot(Loc),
    Minus(Loc),
    Plus(Loc),
    Semicolon(Loc),
    Slash(Loc),
    Star(Loc),

    Bang(Loc),
    BangEqual(Loc),
    Equal(Loc),
    EqualEqual(Loc),
    Greater(Loc),
    GreaterEqual(Loc),
    Less(Loc),
    LessEqual(Loc),

    Identifier(Loc, String),
    String(Loc, String),
    Number(Loc, String),

    And(Loc),
    Break(Loc),
    Class(Loc),
    Else(Loc),
    False(Loc),
    Fun(Loc),
    For(Loc),
    If(Loc),
    Nil(Loc),
    Or(Loc),
    Print(Loc),
    Return(Loc),
    Super(Loc),
    This(Loc),
    True(Loc),
    Var(Loc),
    While(Loc),
}

impl Token {
    pub fn location(&self) -> Loc {
        let loc = match self {
            Token::LeftParen(loc) => loc,
            Token::RightParen(loc) => loc,
            Token::LeftBrace(loc) => loc,
            Token::RightBrace(loc) => loc,
            Token::Comma(loc) => loc,
            Token::Dot(loc) => loc,
            Token::Minus(loc) => loc,
            Token::Plus(loc) => loc,
            Token::Semicolon(loc) => loc,
            Token::Slash(loc) => loc,
            Token::Star(loc) => loc,

            Token::Bang(loc) => loc,
            Token::BangEqual(loc) => loc,
            Token::Equal(loc) => loc,
            Token::EqualEqual(loc) => loc,
            Token::Greater(loc) => loc,
            Token::GreaterEqual(loc) => loc,
            Token::Less(loc) => loc,
            Token::LessEqual(loc) => loc,

            Token::Identifier(loc, _) => loc,
            Token::String(loc, _) => loc,
            Token::Number(loc, _) => loc,

            Token::And(loc) => loc,
            Token::Break(loc) => loc,
            Token::Class(loc) => loc,
            Token::Else(loc) => loc,
            Token::False(loc) => loc,
            Token::Fun(loc) => loc,
            Token::For(loc) => loc,
            Token::If(loc) => loc,
            Token::Nil(loc) => loc,
            Token::Or(loc) => loc,
            Token::Print(loc) => loc,
            Token::Return(loc) => loc,
            Token::Super(loc) => loc,
            Token::This(loc) => loc,
            Token::True(loc) => loc,
            Token::Var(loc) => loc,
            Token::While(loc) => loc,
        };

        loc.clone().with_sample(self.lexeme())
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
        write!(f, "{}", self.location())
    }
}

#[cfg(test)]
mod tests {
    use super::Token;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Token::If(1.into())), "'if' at line 1");
    }
}