use std::io::BufRead;

use crate::token;

pub struct Lexer {
    current_file: String,
    current_input: Vec<u8>,
    current_position: usize,
}

impl Lexer {
    pub fn new(file_path: String) -> Self {
        Lexer {
            current_file: file_path,
            current_input: vec![],
            current_position: 0,
        }
    }

    fn error_at(&self, loc: usize, msg: &str) {
        let mut line_no = 1;
        let mut p = 0;
        while p < self.current_position {
            if self.current_input[p] == b'\n' {
                line_no += 1;
            }
            p += 1;
        }
        self.verror_at(line_no, loc, msg);
    }

    /// Reports an error message in the following format and exit.
    ///
    /// foo.c:10: x = y + 1;
    ///               ^ <error message here>
    fn verror_at(&self, line_no: i32, loc: usize, msg: &str) {
        // Find a line containing `loc`.
        let mut line = loc;
        let code = &self.current_input;
        while self.current_position < line && code[line - 1] != b'\n' {
            line -= 1;
        }

        let mut end = loc;
        while code[end] != b'\n' {
            end += 1;
        }

        // Print out the line
        let line_msg = format!("{}:{}: ", self.current_file, line_no);
        let indent = line_msg.len();
        eprint!("{}", line_msg);
        eprintln!("{}", String::from_utf8(code[line..end].to_vec()).unwrap());

        // Show the error message.
        let pos = loc - line + indent;

        eprint!("{}", " ".repeat(pos));
        eprintln!("^ {}", msg);
        std::process::exit(1);
    }

    /// Returns the contents of a given file.
    pub fn read_file(&mut self) {
        if self.current_file == "-" {
            let stdin = std::io::stdin();
            for line in stdin.lock().lines() {
                self.current_input
                    .append(&mut line.unwrap().as_bytes().to_vec());
                self.current_input.push(b'\n');
            }
        } else {
            self.current_input = std::fs::read_to_string(self.current_file.clone())
                .expect("no such file")
                .as_bytes()
                .to_vec();
            if !self.current_input.ends_with(&[b'\n']) {
                self.current_input.push(b'\n');
            }
        }
    }

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

    fn string_literal_end(&mut self, mut pos: usize) -> usize {
        let start = pos;
        while self.current_input[pos] != b'"' {
            if self.current_input[pos] == b'\n' {
                self.error_at(start, "unclosed string literal");
            }
            if self.current_input[pos] == b'\\' {
                pos += 1;
            }
            pos += 1;
        }
        pos
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

    fn read_string_literal(&mut self, start: usize) -> token::Token {
        let end = self.string_literal_end(start + 1);
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

        token::Token {
            token: token::TokenKind::String(buffer),
            loc: start,
            len: end + 1 - start,
        }
    }

    fn read_punctruator(&mut self) -> Option<token::Token> {
        let start = self.current_position;
        match self.current_input[self.current_position..] {
            [b'=', b'=', ..] => {
                self.current_position += 2;
                let token = token::Token {
                    token: token::TokenKind::EqualEqual,
                    loc: start,
                    len: 2,
                };
                Some(token)
            }
            [b'!', b'=', ..] => {
                self.current_position += 2;
                let token = token::Token {
                    token: token::TokenKind::BangEqual,
                    loc: start,
                    len: 2,
                };
                Some(token)
            }
            [b'>', b'=', ..] => {
                self.current_position += 2;
                let token = token::Token {
                    token: token::TokenKind::GreaterOrEqual,
                    loc: start,
                    len: 2,
                };
                Some(token)
            }
            [b'<', b'=', ..] => {
                self.current_position += 2;
                let token = token::Token {
                    token: token::TokenKind::LessOrEqual,
                    loc: start,
                    len: 2,
                };
                Some(token)
            }
            [b'=', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Equal,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'!', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Bang,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'>', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::GreaterThan,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'<', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::LessThan,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'(', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::OpenParen,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b')', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::CloseParen,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'{', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::OpenBrace,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'}', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::CloseBrace,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'[', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::OpenBracket,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b']', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::CloseBracket,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b',', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Comma,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b';', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Semicolon,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'+', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Plus,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'-', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Minus,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'*', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Star,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            [b'/', ..] => {
                self.current_position += 1;
                let token = token::Token {
                    token: token::TokenKind::Slash,
                    loc: start,
                    len: 1,
                };
                Some(token)
            }
            _ => None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<token::Token> {
        let mut tokens = vec![];
        self.read_file();

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
                });
                continue;
            }

            // String literal
            if self.current_input[self.current_position] == b'"' {
                let token = self.read_string_literal(self.current_position);
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
                    ident => token::TokenKind::Ident(ident.to_string()),
                };
                let token = token::Token {
                    token: token_kind,
                    loc: start,
                    len: self.current_position - start,
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
        };
        tokens.push(eof_token);
        println!("{:?}", tokens);

        tokens
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
