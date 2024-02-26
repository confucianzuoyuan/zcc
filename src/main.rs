#![allow(unused_imports)]
#![allow(dead_code)]

use std::{io::BufReader, rc::Rc};

mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod position;
mod symbol;
mod terminal;
mod token;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args[0]);
    }

    let lexer = lexer::Lexer::new(args[1].as_bytes(), 0);

    let mut symbols = symbol::Symbols::new(Rc::new(symbol::Strings::new()));
    let mut parser = parser::Parser::new(lexer, &mut symbols);

    let ast = parser.parse();

    if let Err(error) = ast {
        let terminal = terminal::Terminal::new();
        if let Err(error) = error.show(&symbols, &terminal) {
            eprintln!("Error printing errors: {}", error);
        }
    } else {
        let node = ast.unwrap();
        codegen::codegen(node);
    }
}
