#![allow(unused_imports)]
#![allow(dead_code)]

use std::{io::BufReader, rc::Rc};

mod lexer;
mod token;
mod ast;
mod types;
mod parser;
mod typecheck;
mod codegen;
mod symbols;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args[0]);
    }

    let lexer = lexer::Lexer::new(args[1].as_bytes());
    let mut parser = parser::Parser::new(lexer);
    let ast = parser.parse();
    let typed_ast = typecheck::typecheck(ast);
    codegen::gen(typed_ast);
}
