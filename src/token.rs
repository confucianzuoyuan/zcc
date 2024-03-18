use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Plus,           // "+"
    Minus,          // "-"
    Star,           // "*"
    Slash,          // "/"
    Ampersand,      // "&"
    OpenParen,      // "("
    CloseParen,     // ")"
    OpenBrace,      // "{"
    CloseBrace,     // "}"
    OpenBracket,    // "["
    CloseBracket,   // "]"
    Equal,          // "="
    EqualEqual,     // "=="
    Bang,           // "!"
    BangEqual,      // "!="
    LessThan,       // "<"
    LessOrEqual,    // "<="
    GreaterThan,    // ">"
    GreaterOrEqual, // ">="
    Semicolon,      // ";"
    Comma,          // ","
    Number(i64),
    Ident(String),
    String(String),
    KWReturn, // "return"
    KWIf,     // "if"
    KWElse,   // "else"
    KWFor,    // "for"
    KWWhile,  // "while"
    KWInt,    // "int"
    KWChar,   // "char"
    KWSizeOf, // "sizeof"
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
                Token::EqualEqual => "==",
                Token::Bang => "!",
                Token::BangEqual => "!=",
                Token::LessOrEqual => "<=",
                Token::LessThan => "<",
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
                Token::KWReturn => "return",
                Token::KWIf => "if",
                Token::KWElse => "else",
                Token::KWFor => "for",
                Token::KWWhile => "while",
                Token::KWInt => "int",
                Token::KWChar => "char",
                Token::KWSizeOf => "sizeof",
                Token::Number(num) => return num.to_string(),
                Token::String(ref string) => return format!("{:?}", string),
                Token::Ident(ref ident) => ident,
                Token::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
