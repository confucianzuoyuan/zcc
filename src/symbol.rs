use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::position;

pub type Symbol = i64;
pub type SymbolWithPos = position::WithPos<Symbol>;

#[derive(Debug)]
pub struct Strings {
    next_symbol: RefCell<Symbol>,
    strings: RefCell<HashMap<Symbol, String>>,
}

impl Strings {
    pub fn new() -> Self {
        Self {
            next_symbol: RefCell::new(0),
            strings: RefCell::new(HashMap::new()),
        }
    }
}

#[derive(Debug)]
pub struct Symbols {
    strings: Rc<Strings>,
}

impl Symbols {
    pub fn new(strings: Rc<Strings>) -> Self {
        let symbols = Self { strings };
        symbols
    }

    pub fn name(&self, symbol: Symbol) -> String {
        self.strings.strings.borrow()[&symbol].to_string()
    }
}