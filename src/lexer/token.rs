use crate::errors::{SourceLocation, source_location};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    ty: TokenType,
    lexeme: &'a str,
    line: usize,
    column: usize,
}

impl<'a> std::error::Error for Token<'a> {}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at '{}' (line {}, column {})", self.ty, self.lexeme, self.line, self.column)
    }
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, lexeme: &'a str, line: usize, column: usize) -> Self {
        Self {
            ty,
            lexeme,
            line,
            column,
        }
    }

    pub fn is(&self, ty: TokenType) -> bool {
        self.ty == ty
    }

    pub fn is_one_of(&self, ty: &[TokenType]) -> bool {
        ty.contains(&self.ty)
    }

    pub fn token_type(&self) -> TokenType {
        self.ty
    }

    pub fn lexeme(&self) -> &'a str {
        self.lexeme
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn location(&self) -> SourceLocation {
        source_location(self.lexeme().to_string(), self.line, self.column)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}