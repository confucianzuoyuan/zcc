use crate::{lexer, token, types};

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

pub struct Parser {
    pub lexer: lexer::Lexer,
    pub tokens: Vec<token::Token>,
    pub locals: Vec<Obj>,
    pub globals: Vec<Obj>,
    pub current_pos: usize,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Self {
        let tokens = lexer.tokenize();
        Self {
            lexer,
            tokens: tokens,
            locals: vec![],
            globals: vec![],
            current_pos: 0,
        }
    }

    fn eat(&mut self, token: token::TokenKind) {
        if self.tokens[self.current_pos].token == token {
            self.current_pos += 1;
        } else {
            self.lexer.error_tok(
                self.tokens[self.current_pos].clone(),
                format!("expected '{}'", token).as_str(),
            );
        }
    }

    fn declspec(&mut self) -> types::Type {
        if self.tokens[self.current_pos].token == token::TokenKind::KWChar {
            self.current_pos += 1;
            return types::Type::CHAR;
        }

        self.eat(token::TokenKind::KWInt);
        types::Type::INT
    }

    pub fn parse(&mut self) -> Vec<Obj> {
        while self.tokens[self.current_pos].token != token::TokenKind::EndOfFile {}
        self.globals.clone()
    }
}
