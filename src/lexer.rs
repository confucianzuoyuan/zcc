use crate::{error, token};

pub type Result<T> = std::result::Result<T, error::Error>;

pub struct Lexer {
    current_input: Vec<u8>,
    current_position: usize,
}

impl Lexer {
    pub fn new(source_code: Vec<u8>) -> Self {
        Lexer {
            current_input: source_code,
            current_position: 0,
        }
    }

    fn error_at(&self, loc: usize, msg: &str) -> error::Error {
        let mut line_no = 1;
        let mut p = 0;
        while p < self.current_position {
            if self.current_input[p] == b'\n' {
                line_no += 1;
            }
            p += 1;
        }
        error::Error {
            line_number: line_no,
            location: loc,
            message: msg.to_string(),
        }
    }

    // pub fn error_tok(&mut self, token: token::Token, msg: &str) {
    //     self.verror_at(token.line_no, token.loc, msg);
    // }

    fn startswith(&self, prefix: &str) -> bool {
        let len = prefix.len();
        if self.current_position + len - 1 < self.current_input.len() {
            if &self.current_input[self.current_position..self.current_position + len]
                == prefix.as_bytes()
            {
                return true;
            }
        }
        false
    }

    fn string_literal_end(&mut self, mut pos: usize) -> Result<usize> {
        let start = pos;
        while self.current_input[pos] != b'"' {
            if self.current_input[pos] == b'\n' {
                return Err(self.error_at(start, "unclosed string literal"));
            }
            if self.current_input[pos] == b'\\' {
                pos += 1;
            }
            pos += 1;
        }
        Ok(pos)
    }

    fn read_escaped_char(&mut self, mut pos: usize) -> (usize, u8) {
        let new_pos;
        if self.current_input[pos] >= b'0' && self.current_input[pos] <= b'7' {
            // Read an octal number
            let mut c = self.current_input[pos] - b'0';
            pos += 1;
            if self.current_input[pos] >= b'0' && self.current_input[pos] <= b'7' {
                c = (c << 3) + (self.current_input[pos] - b'0');
                pos += 1;
                if self.current_input[pos] >= b'0' && self.current_input[pos] <= b'7' {
                    c = (c << 3) + (self.current_input[pos] - b'0');
                    pos += 1;
                }
            }
            new_pos = pos;
            return (new_pos, c);
        }

        if self.current_input[pos] == b'x' {
            // Read a hexadecimal number.
            pos += 1;
            if !(self.current_input[pos] as char).is_ascii_hexdigit() {
                self.error_at(pos, "invalid hex escape sequence");
            }

            let mut c = 0;
            while (self.current_input[pos] as char).is_ascii_hexdigit() {
                c = (c << 4) + from_hex(self.current_input[pos]);
                pos += 1;
            }
            new_pos = pos;
            return (new_pos, c);
        }

        new_pos = pos + 1;

        // Escape sequences are defined using themselves here. E.g.
        // '\n' is implemented using '\n'. This tautological definition
        // works because the compiler that compiles our compiler knows
        // what '\n' actually is. In other words, we "inherit" the ASCII
        // code of '\n' from the compiler that compiles our compiler,
        // so we don't have to teach the actual code here.
        //
        // This fact has huge implications not only for the correctness
        // of the compiler but also for the security of the generated code.
        // For more info, read "Reflections on Trusting Trust" by Ken Thompson.
        // https://github.com/rui314/chibicc/wiki/thompson1984.pdf
        let c = match self.current_input[pos] {
            b'a' => b'\x07',
            b'b' => b'\x08',
            b't' => b'\t',
            b'n' => b'\n',
            b'v' => b'\x0B',
            b'f' => b'\x0C',
            b'r' => b'\r',
            // [GNU] \e for the ASCII escape character is a GNU C extension.
            b'e' => 27,
            _ => self.current_input[pos],
        };

        (new_pos, c)
    }

    fn read_string_literal(&mut self, start: usize) -> Result<token::Token> {
        let end = self.string_literal_end(start + 1)?;
        let mut buffer = String::new();
        let mut pos = start + 1;
        while pos < end {
            if self.current_input[pos] == b'\\' {
                let (new_pos, c) = self.read_escaped_char(pos + 1);
                pos = new_pos;
                buffer.push(c as char);
            } else {
                buffer.push(self.current_input[pos] as char);
                pos += 1;
            }
        }

        Ok(token::Token {
            token: token::TokenKind::String(buffer),
            loc: start,
            len: end + 1 - start,
            line_no: 0,
        })
    }

    fn make_token(&mut self, token_kind: token::TokenKind, len: usize) -> token::Token {
        let start = self.current_position;
        self.current_position += len;
        let token = token::Token {
            token: token_kind,
            loc: start,
            len,
            line_no: 0,
        };
        token
    }

    // Initialize line info for all tokens.
    fn add_line_numbers(&mut self, tokens: &mut Vec<token::Token>) {
        let mut pos = 0;
        let mut n = 1;

        let mut i = 0;
        loop {
            if pos == tokens[i].loc {
                tokens[i].line_no = n;
                i += 1;
            }
            if self.current_input[pos] == b'\n' {
                n += 1;
            }
            if pos < self.current_input.len() - 1 {
                pos += 1;
            } else {
                break;
            }
        }
    }

    fn read_punctruator(&mut self) -> Option<token::Token> {
        use token::TokenKind::*;
        match self.current_input[self.current_position..] {
            [b'=', b'=', ..] => Some(self.make_token(EqualEqual, 2)),
            [b'!', b'=', ..] => Some(self.make_token(BangEqual, 2)),
            [b'>', b'=', ..] => Some(self.make_token(GreaterOrEqual, 2)),
            [b'<', b'=', ..] => Some(self.make_token(LessOrEqual, 2)),
            [b'=', ..] => Some(self.make_token(Equal, 1)),
            [b'!', ..] => Some(self.make_token(Bang, 1)),
            [b'>', ..] => Some(self.make_token(GreaterThan, 1)),
            [b'<', ..] => Some(self.make_token(LessThan, 1)),
            [b'(', ..] => Some(self.make_token(OpenParen, 1)),
            [b')', ..] => Some(self.make_token(CloseParen, 1)),
            [b'{', ..] => Some(self.make_token(OpenBrace, 1)),
            [b'}', ..] => Some(self.make_token(CloseBrace, 1)),
            [b'[', ..] => Some(self.make_token(OpenBracket, 1)),
            [b']', ..] => Some(self.make_token(CloseBracket, 1)),
            [b',', ..] => Some(self.make_token(Comma, 1)),
            [b';', ..] => Some(self.make_token(Semicolon, 1)),
            [b'+', ..] => Some(self.make_token(Plus, 1)),
            [b'-', ..] => Some(self.make_token(Minus, 1)),
            [b'*', ..] => Some(self.make_token(Star, 1)),
            [b'/', ..] => Some(self.make_token(Slash, 1)),
            _ => None,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<token::Token>> {
        let mut tokens = vec![];

        while self.current_position < self.current_input.len() {
            // Skip line comments
            if self.startswith("//") {
                self.current_position += 2;
                while self.current_input[self.current_position] != b'\n' {
                    self.current_position += 1;
                }
                continue;
            }

            // Skip block comments.
            if self.startswith("/*") {
                let loc = self.current_position;
                self.current_position += 2;
                while self.startswith("*/") {
                    self.current_position += 1;
                    if self.current_position == self.current_input.len() - 1 {
                        self.error_at(loc, "unclosed block comment");
                    }
                }
                self.current_position += 2;
                continue;
            }

            // Skip whitespace characters.
            if (self.current_input[self.current_position] as char).is_whitespace() {
                self.current_position += 1;
                continue;
            }

            // Numeric literal
            if (self.current_input[self.current_position] as char).is_numeric() {
                let start = self.current_position;
                let mut buffer = String::new();
                buffer.push(self.current_input[self.current_position] as char);
                self.current_position += 1;
                while (self.current_input[self.current_position] as char).is_numeric() {
                    buffer.push(self.current_input[self.current_position] as char);
                    self.current_position += 1;
                }
                let end = self.current_position;
                tokens.push(token::Token {
                    token: token::TokenKind::Number(buffer.parse().unwrap()),
                    loc: start,
                    len: end - start,
                    line_no: 0,
                });
                continue;
            }

            // String literal
            if self.current_input[self.current_position] == b'"' {
                let token = self.read_string_literal(self.current_position)?;
                self.current_position += token.len;
                tokens.push(token);
                continue;
            }

            // Identifier or keyword
            if is_ident1(self.current_input[self.current_position]) {
                let start = self.current_position;
                let mut buffer = String::new();
                buffer.push(self.current_input[self.current_position] as char);
                self.current_position += 1;
                while is_ident2(self.current_input[self.current_position]) {
                    buffer.push(self.current_input[self.current_position] as char);
                    self.current_position += 1;
                }

                let token_kind = match buffer.as_str() {
                    "return" => token::TokenKind::KWReturn,
                    "if" => token::TokenKind::KWIf,
                    "else" => token::TokenKind::KWElse,
                    "for" => token::TokenKind::KWFor,
                    "while" => token::TokenKind::KWWhile,
                    "int" => token::TokenKind::KWInt,
                    "sizeof" => token::TokenKind::KWSizeOf,
                    "char" => token::TokenKind::KWChar,
                    "struct" => token::TokenKind::KWStruct,
                    "continue" => token::TokenKind::KWContinue,
                    "static" => token::TokenKind::KWStatic,
                    "extern" => token::TokenKind::KWExtern,
                    "signed" => token::TokenKind::KWSigned,
                    "unsigned" => token::TokenKind::KWUnsigned,
                    "double" => token::TokenKind::KWDouble,
                    "void" => token::TokenKind::KWVoid,
                    "do" => token::TokenKind::KWDo,
                    "long" => token::TokenKind::KWLong,
                    "break" => token::TokenKind::KWBreak,
                    ident => token::TokenKind::Ident(ident.to_string()),
                };
                let token = token::Token {
                    token: token_kind,
                    loc: start,
                    len: self.current_position - start,
                    line_no: 0,
                };
                tokens.push(token);
                continue;
            }

            // Punctuators
            if let Some(token) = self.read_punctruator() {
                tokens.push(token);
                continue;
            }

            self.error_at(self.current_position, "invalid token");
        }

        let eof_token = token::Token {
            token: token::TokenKind::EndOfFile,
            loc: self.current_position,
            len: 0,
            line_no: 0,
        };
        tokens.push(eof_token);
        self.add_line_numbers(&mut tokens);

        Ok(tokens)
    }
}

fn from_hex(c: u8) -> u8 {
    if c >= b'0' && c <= b'9' {
        return c - b'0';
    }
    if c >= b'a' && c <= b'f' {
        return c - b'a' + 10;
    }
    c - b'A' + 10
}

// Returns true if c is valid as the first character of an identifier.
fn is_ident1(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
}

// Returns true if c is valid as a non-first character of an identifier.
fn is_ident2(c: u8) -> bool {
    is_ident1(c) || (c >= b'0' && c <= b'9')
}
