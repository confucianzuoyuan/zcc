use crate::{
    context::Context,
    file::FileIndex,
    tok::{self, Token},
    unicode::{decode_utf8, is_identifier, is_identifier_start},
};

pub struct Tokenizer {
    /// context
    pub context: Context,
    /// Input file
    pub current_file: FileIndex,
    /// True if the current position is at the beginning of a line
    pub at_bol: bool,
    /// True if the current position follows a space character
    pub has_space: bool,
    pub token_index: usize,
}

impl Tokenizer {
    /// Create a new tokenizer.
    pub fn new(context: Context) -> Self {
        Tokenizer {
            context,
            current_file: 0,
            at_bol: false,
            has_space: false,
            token_index: 0,
        }
    }

    /// check current token matches op or not.
    pub fn equal(&self, tok: &Token, op: &str) -> bool {
        let src = self
            .context
            .get_file(self.current_file)
            .expect("msg")
            .contents;
        for i in 0..op.len() {
            if src[tok.loc + i] != op.as_bytes()[i] {
                return false;
            }
        }
        true
    }

    /// consume token
    pub fn consume(&mut self, tok: &Token, str: &str) -> bool {
        if self.equal(tok, str) {
            self.token_index += 1;
            return true;
        }
        false
    }

    pub fn startswith(&self, p: usize, q: &str) -> bool {
        let src = self
            .context
            .get_file(self.current_file)
            .expect("msg")
            .contents;
        for i in 0..q.len() {
            if src[p + i] != q.as_bytes()[i] {
                return false;
            }
        }
        true
    }

    // Read an identifier and returns the length of it.
    // If p does not point to a valid identifier, 0 is returned.
    pub fn read_identifier(&self, start: usize) -> usize {
        let src = self
            .context
            .get_file(self.current_file)
            .expect("msg")
            .contents;
        let mut p = start;
        let c = decode_utf8(&src[..], start);
        p = c.1;
        if is_identifier_start(c.0) {
            return 0;
        }

        loop {
            let c = decode_utf8(&src[..], p);
            if !is_identifier(c.0) {
                return c.1;
            }
            p = c.1;
            if p >= src.len() {
                break;
            }
        }
        0
    }

    pub fn is_keyword(&self, tok: &Token) -> bool {
        let src = self
            .context
            .get_file(self.current_file)
            .expect("msg")
            .contents;
        let keywords = [
            "return",
            "if",
            "else",
            "for",
            "while",
            "int",
            "sizeof",
            "char",
            "struct",
            "union",
            "short",
            "long",
            "void",
            "typedef",
            "_Bool",
            "enum",
            "static",
            "goto",
            "break",
            "continue",
            "switch",
            "case",
            "default",
            "extern",
            "_Alignof",
            "_Alignas",
            "do",
            "signed",
            "unsigned",
            "const",
            "volatile",
            "auto",
            "register",
            "restrict",
            "__restrict",
            "__restrict__",
            "_Noreturn",
            "float",
            "double",
            "typeof",
            "asm",
            "_Thread_local",
            "__thread",
            "_Atomic",
            "__attribute__",
        ];
        for keyword in keywords.iter() {
            if self.equal(tok, keyword) {
                return true;
            }
        }
        false
    }

    /// Read a punctuator token from p and returns its length.
    pub fn read_punctuator(&self, p: usize) -> usize {
        let src = self
            .context
            .get_file(self.current_file)
            .expect("msg")
            .contents;
        let punctuators = [
            "<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--",
            "%=", "&=", "|=", "^=", "&&", "||", "<<", ">>", "##",
        ];
        for punctuator in punctuators.iter() {
            if self.startswith(p, punctuator) {
                return punctuator.len();
            }
        }
        let single_punctuators = [
            "(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "?", "+", "-", "*", "/", "%", "&",
            "|", "^", "~", "!", "<", ">", "=", "#",
        ];
        for single_punctuator in single_punctuators.iter() {
            if self.startswith(p, single_punctuator) {
                return single_punctuator.len();
            }
        }
        0
    }
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use crate::file::File;

    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(1, 2), 3);
    }

    #[test]
    fn test_lexer() {
        let mut context = Context::new();
        context.add_file(File::new("name", 0, "int main()".as_bytes()));
        let mut tokenizer = Tokenizer::new(context);
        let len = tokenizer.read_identifier(0);
        assert_eq!(len, 3);
    }
}
