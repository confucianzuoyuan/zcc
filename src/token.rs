use std::fmt::Display;

use crate::position;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Plus,
    Minus,
    Number(i64),
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
                Tok::Number(num) => return num.to_string(),
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
