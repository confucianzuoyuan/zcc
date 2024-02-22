use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::terminal;

pub type Symbol = i64;

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

#[derive(Clone, Copy, Debug)]
pub struct Pos {
    pub byte: u64,
    pub column: u32,
    pub file: Symbol,
    pub length: usize,
    pub line: u32,
}

impl Pos {
    pub fn new(line: u32, column: u32, byte: u64, file: Symbol, length: usize) -> Self {
        Pos {
            byte,
            column,
            file,
            length,
            line,
        }
    }

    pub fn dummy() -> Self {
        Self::new(u32::MAX, u32::MAX, u64::MAX, 0, 0)
    }

    pub fn grow(&self, pos: Pos) -> Self {
        Pos {
            byte: self.byte,
            column: self.column,
            file: self.file,
            length: (pos.byte - self.byte) as usize + pos.length,
            line: self.line,
        }
    }

    pub fn show(&self, symbols: &Symbols, terminal: &terminal::Terminal) {
        let filename = symbols.name(self.file);
        eprintln!(
            "   {}{}-->{}{} {}:{}:{}",
            terminal.bold(),
            terminal.blue(),
            terminal.reset_color(),
            terminal.end_bold(),
            filename,
            self.line,
            self.column
        )
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:", self.line, self.column)
    }
}
