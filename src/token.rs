use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Equal,
    DoubleEqual,
    Bang,
    BangEqual,
    LesserThan,
    LesserOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Semicolon,
    Comma,
    Number(i64),
    Ident(String),
    Return,
    If,
    Else,
    For,
    While,
    KWInt,
    EndOfFile,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Slash => "/",
                Token::Star => "*",
                Token::Ampersand => "&",
                Token::Equal => "=",
                Token::DoubleEqual => "==",
                Token::Bang => "!",
                Token::BangEqual => "!=",
                Token::LesserOrEqual => "<=",
                Token::LesserThan => "<",
                Token::GreaterOrEqual => ">=",
                Token::GreaterThan => ">",
                Token::OpenParen => "(",
                Token::CloseParen => ")",
                Token::OpenBrace => "{",
                Token::CloseBrace => "}",
                Token::OpenBracket => "[",
                Token::CloseBracket => "]",
                Token::Semicolon => ";",
                Token::Comma => ",",
                Token::Return => "return",
                Token::If => "if",
                Token::Else => "else",
                Token::For => "for",
                Token::While => "while",
                Token::KWInt => "int",
                Token::Number(num) => return num.to_string(),
                Token::Ident(ref ident) => ident,
                Token::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
