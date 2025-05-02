use crate::{file::File, tok::Token};

#[derive(Debug)]
pub struct Context {
    pub input_files: Vec<File>,
    pub tokens: Vec<Token>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            input_files: Vec::new(),
            tokens: Vec::new(),
        }
    }

    /// Add a new file to the context.
    pub fn add_file(&mut self, file: File) {
        self.input_files.push(file);
    }

    /// Add a new token to the context.
    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn get_file(&self, index: usize) -> Option<&File> {
        self.input_files.get(index)
    }
}
