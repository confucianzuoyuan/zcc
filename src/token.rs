use std::fmt::Display;

use crate::position;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Equal,
    DoubleEqual,
    Bang,
    BangEqual,
    LesserThan,
    LesserOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Semicolon,
    Number(i64),
    Ident(String),
    Return,
    EndOfFile,
}

#[derive(Debug)]
pub struct Token {
    pub pos: position::Pos,
    pub token: Tok,
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                Tok::Plus => "+",
                Tok::Minus => "-",
                Tok::Slash => "/",
                Tok::Star => "*",
                Tok::Equal => "=",
                Tok::DoubleEqual => "==",
                Tok::Bang => "!",
                Tok::BangEqual => "!=",
                Tok::LesserOrEqual => "<=",
                Tok::LesserThan => "<",
                Tok::GreaterOrEqual => ">=",
                Tok::GreaterThan => ">",
                Tok::OpenParen => "(",
                Tok::CloseParen => ")",
                Tok::OpenBrace => "{",
                Tok::CloseBrace => "}",
                Tok::Semicolon => ";",
                Tok::Return => "return",
                Tok::Number(num) => return num.to_string(),
                Tok::Ident(ref ident) => ident,
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
