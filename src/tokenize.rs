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
        let c = decode_utf8(&src[p..], start);
        p += c.1;
        if is_identifier_start(c.0) {
            return 0;
        }

        loop {
            let c = decode_utf8(&src[p..], p);
            p += c.1;
            if !is_identifier(c.0) {
                return c.1 - start;
            }
        }
        0
    }
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
