use crate::token;

use std::io::{Bytes, Read};
use std::iter::Peekable;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Lexer {
            bytes_iter: reader.bytes().peekable(),
        }
    }

    fn advance(&mut self) {
        self.bytes_iter.next();
    }

    fn eat(&mut self, ch: char) {
        if self.current_char() != ch {
            panic!(
                "Expected character `{}`, but found `{}`.",
                ch,
                self.current_char()
            );
        }
        self.advance()
    }

    pub fn integer(&mut self) -> token::Token {
        let buffer = self.take_while(char::is_numeric);
        let num = buffer.parse().unwrap();
        token::Token::Number(num)
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut buffer = String::new();
        buffer.push(self.current_char());
        self.advance();
        let mut ch = self.current_char();
        while pred(ch) {
            buffer.push(ch);
            self.advance();
            ch = self.current_char();
        }
        buffer
    }

    fn simple_token(&mut self, token: token::Token) -> token::Token {
        self.advance();
        token
    }

    fn current_char(&mut self) -> char {
        if let Some(&Ok(byte)) = self.bytes_iter.peek() {
            byte as char
        } else {
            panic!();
        }
    }

    fn two_char_token(
        &mut self,
        tokens: Vec<(char, token::Token)>,
        default: token::Token,
    ) -> token::Token {
        self.advance();
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
        let token = if let Some(token) = token {
            self.advance();
            token
        } else {
            default
        };
        token
    }

    /// "<" or "<="
    fn lesser_or_lesser_euqal(&mut self) -> token::Token {
        self.two_char_token(
            vec![('=', token::Token::LessOrEqual)],
            token::Token::LessThan,
        )
    }

    /// ">" or ">="
    fn greater_or_greater_euqal(&mut self) -> token::Token {
        self.two_char_token(
            vec![('=', token::Token::GreaterOrEqual)],
            token::Token::GreaterThan,
        )
    }

    /// "==" or "="
    fn equal_or_double_equal(&mut self) -> token::Token {
        self.two_char_token(vec![('=', token::Token::EqualEqual)], token::Token::Equal)
    }

    /// "!=" or "!"
    fn bang_or_bang_equal(&mut self) -> token::Token {
        self.two_char_token(vec![('=', token::Token::BangEqual)], token::Token::Bang)
    }

    fn identifier(&mut self) -> token::Token {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_');
        let token = match ident.as_str() {
            "return" => token::Token::KWReturn,
            "if" => token::Token::KWIf,
            "else" => token::Token::KWElse,
            "for" => token::Token::KWFor,
            "while" => token::Token::KWWhile,
            "int" => token::Token::KWInt,
            "char" => token::Token::KWChar,
            "sizeof" => token::Token::KWSizeOf,
            "unsigned" => token::Token::KWUnsigned,
            "switch" => token::Token::KWSwitch,
            "do" => token::Token::KWDo,
            _ => token::Token::Ident(ident),
        };
        token
    }

    fn escape_char(&mut self) -> char {
        let escaped_char = match self.current_char() {
            'n' => '\n',
            't' => '\t',
            'b' => '\x08',
            'v' => '\x0B',
            'f' => '\x0C',
            'r' => '\r',
            'a' => '\x07',
            // [GNU] \e for the ASCII escape character is a GNU C extension.
            'e' => '\x1B',
            _ => self.current_char(),
        };
        self.advance();
        escaped_char
    }

    fn string(&mut self) -> token::Token {
        let mut string = String::new();
        self.eat('"');
        let mut ch = self.current_char();
        while ch != '"' {
            if ch == '\\' {
                self.advance();
                string.push(self.escape_char());
            } else {
                string.push(ch);
                self.advance();
            }
            ch = self.current_char();
        }
        self.eat('"');
        token::Token::String(string)
    }

    pub fn token(&mut self) -> token::Token {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
                b'0'..=b'9' => self.integer(),
                b' ' | b'\n' | b'\t' => {
                    self.advance();
                    self.token()
                }
                b'+' => self.simple_token(token::Token::Plus),
                b'-' => self.simple_token(token::Token::Minus),
                b'*' => self.simple_token(token::Token::Star),
                b'/' => self.simple_token(token::Token::Slash),
                b'(' => self.simple_token(token::Token::OpenParen),
                b')' => self.simple_token(token::Token::CloseParen),
                b'<' => self.lesser_or_lesser_euqal(),
                b'>' => self.greater_or_greater_euqal(),
                b'!' => self.bang_or_bang_equal(),
                b'=' => self.equal_or_double_equal(),
                b';' => self.simple_token(token::Token::Semicolon),
                b',' => self.simple_token(token::Token::Comma),
                b'{' => self.simple_token(token::Token::OpenBrace),
                b'}' => self.simple_token(token::Token::CloseBrace),
                b'[' => self.simple_token(token::Token::OpenBracket),
                b']' => self.simple_token(token::Token::CloseBracket),
                b'&' => self.simple_token(token::Token::Ampersand),
                b'"' => self.string(),
                _ => panic!("unknown token"),
            };
        }
        token::Token::EndOfFile
    }
}
