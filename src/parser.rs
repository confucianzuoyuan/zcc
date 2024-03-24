use crate::{token, types};

#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    LocalVariable {
        name: String,
        ty: types::Type,
        offset: i64,
    },
    GlobalVariable {
        name: String,
        ty: types::Type,
        init_data: String,
    },
    Function {
        name: String,
        ty: types::Type,
        params: Vec<Obj>,
        locals: Vec<Obj>,
        stack_size: i64,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parser {
    pub tokens: Vec<token::Token>,
    pub locals: Vec<Obj>,
    pub globals: Vec<Obj>,
    pub current_pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Self {
            tokens,
            locals: vec![],
            globals: vec![],
            current_pos: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Obj> {
        while self.tokens[self.current_pos].token != token::TokenKind::EndOfFile {
            
        }
        self.globals.clone()
    }
}
