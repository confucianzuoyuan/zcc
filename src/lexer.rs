use crate::{error, position, symbol, token};

use std::io::{Bytes, Read};
use std::iter::Peekable;

pub type Result<T> = std::result::Result<T, error::Error>;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
    pos: position::Pos,
    saved_pos: position::Pos,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, filename: symbol::Symbol) -> Self {
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
                self.pos.column += 1;
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

    pub fn integer(&mut self) -> Result<token::Token> {
        let buffer = self.take_while(char::is_numeric)?;
        let num = buffer.parse().unwrap();
        self.make_token(token::Tok::Number(num), error::num_text_size(num))
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
        self.save_start();
        let mut buffer = String::new();
        buffer.push(self.current_char()?);
        self.advance()?;
        // match self.current_char() {
        //     Ok(_) => (),
        //     Err(_) => return Ok(buffer),
        // };
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            // match self.current_char() {
            //     Ok(_) => (),
            //     Err(_) => break,
            // };
            ch = self.current_char()?;
        }
        Ok(buffer)
    }

    fn save_start(&mut self) {
        self.saved_pos = self.current_pos();
    }

    fn simple_token(&mut self, token: token::Tok) -> Result<token::Token> {
        let mut pos = self.pos;
        pos.length = 1;
        self.advance()?;
        Ok(token::Token { pos, token })
    }

    fn current_pos(&self) -> position::Pos {
        self.pos
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

    fn make_token(&self, token: token::Tok, length: usize) -> Result<token::Token> {
        if length > 10000 {
            panic!();
        }
        let mut pos = self.saved_pos;
        pos.length = length;
        Ok(token::Token { pos, token })
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
    fn lesser_or_lesser_euqal(&mut self) -> Result<token::Token> {
        self.two_char_token(
            vec![('=', token::Tok::LesserOrEqual)],
            token::Tok::LesserThan,
        )
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
        self.two_char_token(vec![('=', token::Tok::DoubleEqual)], token::Tok::Equal)
    }

    /// "!=" or "!"
    fn bang_or_bang_equal(&mut self) -> Result<token::Token> {
        self.two_char_token(vec![('=', token::Tok::BangEqual)], token::Tok::Bang)
    }

    fn identifier(&mut self) -> Result<token::Token> {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_')?;
        let len = ident.len();
        let token = match ident.as_str() {
            "return" => token::Tok::Return,
            _ => token::Tok::Ident(ident),
        };
        self.make_token(token, len)
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
                b'+' => self.simple_token(token::Tok::Plus),
                b'-' => self.simple_token(token::Tok::Minus),
                b'*' => self.simple_token(token::Tok::Star),
                b'/' => self.simple_token(token::Tok::Slash),
                b'(' => self.simple_token(token::Tok::OpenParen),
                b')' => self.simple_token(token::Tok::CloseParen),
                b'<' => self.lesser_or_lesser_euqal(),
                b'>' => self.greater_or_greater_euqal(),
                b'!' => self.bang_or_bang_equal(),
                b'=' => self.equal_or_double_equal(),
                b';' => self.simple_token(token::Tok::Semicolon),
                b'{' => self.simple_token(token::Tok::OpenBrace),
                b'}' => self.simple_token(token::Tok::CloseBrace),
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
