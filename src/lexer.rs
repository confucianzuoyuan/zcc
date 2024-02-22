use crate::{error, position, token};

use std::io::{Bytes, Read};
use std::iter::Peekable;

pub type Result<T> = std::result::Result<T, error::Error>;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
    pos: position::Pos,
    saved_pos: position::Pos,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, filename: position::Symbol) -> Self {
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
        match self.current_char() {
            Ok(_) => (),
            Err(_) => return Ok(buffer),
        };
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            match self.current_char() {
                Ok(c) => ch = c,
                Err(_) => break,
            }
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

    pub fn token(&mut self) -> Result<token::Token> {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b'0'..=b'9' => self.integer(),
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'+' => self.simple_token(token::Tok::Plus),
                b'-' => self.simple_token(token::Tok::Minus),
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
