use crate::{error, position, symbols, token};

use std::io::{Bytes, Read};
use std::iter::Peekable;

pub type Result<T> = std::result::Result<T, error::Error>;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
    pos: position::Pos,
    saved_pos: position::Pos,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, filename: symbols::Symbol) -> Self {
        Lexer {
            bytes_iter: reader.bytes().peekable(),
            pos: position::Pos::new(1, 1, 0, filename, 0),
            saved_pos: position::Pos::new(1, 1, 0, filename, 0),
        }
    }

    fn advance(&mut self) -> Result<()> {
        match self.bytes_iter.next() {
            Some(Ok(b'\n')) => {
                self.pos.line += 1;
                self.pos.column = 1;
                self.pos.byte += 1;
            }
            Some(Err(error)) => return Err(error.into()),
            None => return Err(error::Error::Eof),
            _ => {
                self.pos.column += 1;
                self.pos.byte += 1;
            }
        }
        Ok(())
    }

    fn eat(&mut self, ch: char) -> Result<()> {
        if self.current_char()? != ch {
            panic!(
                "Expected character `{}`, but found `{}`.",
                ch,
                self.current_char()?
            );
        }
        self.advance()
    }

    pub fn integer(&mut self) -> Result<token::Token> {
        let buffer = self.take_while(char::is_numeric)?;
        // the buffer only contains digit, hence unwrap().
        let num = buffer.parse().unwrap();
        self.make_token(token::Tok::Number(num), error::num_text_size(num))
    }

    fn make_token(&self, token: token::Tok, length: usize) -> Result<token::Token> {
        if length > 10000 {
            panic!();
        }
        let mut pos = self.saved_pos;
        pos.length = length;
        Ok(token::Token { pos, token })
    }

    fn current_pos(&self) -> position::Pos {
        self.pos
    }

    fn save_start(&mut self) {
        self.saved_pos = self.current_pos();
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
        self.save_start();
        let mut buffer = String::new();
        buffer.push(self.current_char()?);
        self.advance()?;
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            ch = self.current_char()?;
        }
        Ok(buffer)
    }

    fn simple_token(&mut self, token: token::Tok) -> Result<token::Token> {
        let mut pos = self.pos;
        pos.length = 1;
        self.advance()?;
        Ok(token::Token { pos, token })
    }

    fn current_char(&mut self) -> Result<char> {
        if let Some(&Ok(byte)) = self.bytes_iter.peek() {
            return Ok(byte as char);
        }

        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => Err(error::Error::Eof),
        }
    }

    fn two_char_token(
        &mut self,
        tokens: Vec<(char, token::Tok)>,
        default: token::Tok,
    ) -> Result<token::Token> {
        self.save_start();
        self.advance()?;
        let token = match self.bytes_iter.peek() {
            Some(&Ok(byte)) => {
                let mut token = None;
                let next_char = byte as char;
                for (ch, tok) in tokens {
                    if ch == next_char {
                        token = Some(tok);
                    }
                }
                token
            }
            _ => None,
        };
        let (token, len) = if let Some(token) = token {
            self.advance()?;
            (token, 2)
        } else {
            (default, 1)
        };
        self.make_token(token, len)
    }

    /// "<" or "<="
    fn less_or_less_euqal(&mut self) -> Result<token::Token> {
        self.two_char_token(vec![('=', token::Tok::LessOrEqual)], token::Tok::LessThan)
    }

    /// ">" or ">="
    fn greater_or_greater_euqal(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![('=', token::Tok::GreaterOrEqual)],
            token::Tok::GreaterThan,
        )
    }

    /// "==" or "="
    fn equal_or_double_equal(&mut self) -> Result<token::Token> {
        self.two_char_token(vec![('=', token::Tok::EqualEqual)], token::Tok::Equal)
    }

    /// "!=" or "!"
    fn bang_or_bang_equal(&mut self) -> Result<token::Token> {
        self.two_char_token(vec![('=', token::Tok::BangEqual)], token::Tok::Bang)
    }

    /// "&&" or "&=" or "&"
    fn and_or_other(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![('=', token::Tok::AndEqual), ('&', token::Tok::AndAnd)],
            token::Tok::And,
        )
    }

    /// "||" or "|=" or "|"
    fn or_or_other(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![('=', token::Tok::OrEqual), ('|', token::Tok::OrOr)],
            token::Tok::Or,
        )
    }

    /// "++" or "+=" or "+"
    fn plus_or_other(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![('+', token::Tok::PlusPlus), ('=', token::Tok::PlusEqual)],
            token::Tok::Plus,
        )
    }

    /// "--" or "-=" or "-" or "->"
    fn minus_or_other(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![
                ('-', token::Tok::MinusMinus),
                ('=', token::Tok::MinusEqual),
                ('>', token::Tok::MinusGreater),
            ],
            token::Tok::Minus,
        )
    }

    /// "*=" or "*"
    fn star_or_other(&mut self) -> Result<token::Token> {
        self.two_char_token(vec![('=', token::Tok::StarEqual)], token::Tok::Star)
    }

    fn identifier(&mut self) -> Result<token::Token> {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_')?;
        let len = ident.len();
        let token = match ident.as_str() {
            "return" => token::Tok::KWReturn,
            "if" => token::Tok::KWIf,
            "else" => token::Tok::KWElse,
            "for" => token::Tok::KWFor,
            "while" => token::Tok::KWWhile,
            "int" => token::Tok::KWInt,
            "char" => token::Tok::KWChar,
            "sizeof" => token::Tok::KWSizeOf,
            "unsigned" => token::Tok::KWUnsigned,
            "switch" => token::Tok::KWSwitch,
            "case" => token::Tok::KWCase,
            "do" => token::Tok::KWDo,
            "void" => token::Tok::KWVoid,
            "_Noreturn" => token::Tok::KWUnderscoreNoReturn,
            _ => token::Tok::Ident(ident),
        };
        self.make_token(token, len)
    }

    fn escape_char(&mut self, mut pos: position::Pos) -> Result<char> {
        let escaped_char = match self.current_char()? {
            'n' => '\n',
            't' => '\t',
            'b' => '\x08',
            'v' => '\x0B',
            'f' => '\x0C',
            'r' => '\r',
            'a' => '\x07',
            // [GNU] \e for the ASCII escape character is a GNU C extension.
            'e' => '\x1B',
            escape => {
                pos.length = 2;
                return Err(error::Error::InvalidEscape {
                    escape: escape.to_string(),
                    pos,
                });
            }
        };
        self.advance()?;
        Ok(escaped_char)
    }

    fn slash_or_other(&mut self) -> Result<token::Token> {
        self.save_start();
        self.advance()?;
        if self.current_char()? == '*' {
            match self.comment() {
                Err(error::Error::Eof) => {
                    let mut pos = self.saved_pos;
                    pos.length = 2;
                    return Err(error::Error::Unclosed {
                        pos,
                        token: "comment",
                    });
                }
                Err(error) => return Err(error),
                _ => (),
            }
            self.token()
        } else if self.current_char()? == '/' {
            loop {
                self.advance()?;
                if self.current_char()? == '\n' {
                    break;
                }
            }
            self.token()
        } else if self.current_char()? == '=' {
            self.advance()?;
            self.make_token(token::Tok::SlashEqual, 2)
        } else {
            self.make_token(token::Tok::Slash, 1)
        }
    }

    fn comment(&mut self) -> Result<()> {
        let mut depth = 1;
        loop {
            self.advance()?;
            let ch = self.current_char()?;
            // check for nested comment.
            if ch == '/' {
                self.advance()?;
                let ch = self.current_char()?;
                if ch == '*' {
                    depth += 1;
                }
            }
            // check for end of comment
            if ch == '*' {
                self.advance()?;
                let ch = self.current_char()?;
                if ch == '/' {
                    depth -= 1;
                    if depth == 0 {
                        self.advance()?;
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    fn string(&mut self) -> Result<token::Token> {
        let result = (|| {
            self.save_start();
            let mut string = String::new();
            let start = self.current_pos().byte;
            self.eat('"')?;
            let mut ch = self.current_char()?;
            while ch != '"' {
                // look for escaped character.
                if ch == '\\' {
                    let pos = self.current_pos();
                    self.advance()?;
                    string.push(self.escape_char(pos)?);
                } else {
                    string.push(ch);
                    self.advance()?;
                }
                ch = self.current_char()?;
            }
            self.eat('"')?;
            let len = self.current_pos().byte - start;
            self.make_token(token::Tok::String(string), len as usize)
        })();
        match result {
            Err(error::Error::Eof) => {
                let mut pos = self.saved_pos;
                pos.length = 1;
                Err(error::Error::Unclosed {
                    pos,
                    token: "string",
                })
            }
            _ => result,
        }
    }

    pub fn token(&mut self) -> Result<token::Token> {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
                b'0'..=b'9' => self.integer(),
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'+' => self.plus_or_other(),
                b'-' => self.minus_or_other(),
                b'*' => self.star_or_other(),
                b'/' => self.slash_or_other(),
                b'(' => self.simple_token(token::Tok::OpenParen),
                b')' => self.simple_token(token::Tok::CloseParen),
                b'<' => self.less_or_less_euqal(),
                b'>' => self.greater_or_greater_euqal(),
                b'!' => self.bang_or_bang_equal(),
                b'=' => self.equal_or_double_equal(),
                b';' => self.simple_token(token::Tok::Semicolon),
                b',' => self.simple_token(token::Tok::Comma),
                b'{' => self.simple_token(token::Tok::OpenBrace),
                b'}' => self.simple_token(token::Tok::CloseBrace),
                b'[' => self.simple_token(token::Tok::OpenBracket),
                b']' => self.simple_token(token::Tok::CloseBracket),
                b'&' => self.and_or_other(),
                b'|' => self.or_or_other(),
                b'"' => self.string(),
                _ => {
                    let mut pos = self.current_pos();
                    pos.length = 1;
                    Err(error::Error::UnknownToken {
                        pos,
                        start: ch as char,
                    })
                }
            };
        }
        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => {
                let mut pos = self.pos;
                pos.length = 1;
                Ok(token::Token {
                    pos,
                    token: token::Tok::EndOfFile,
                })
            }
        }
    }
}
