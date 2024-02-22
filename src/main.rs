#![allow(unused_imports)]
#![allow(dead_code)]

use std::io::BufReader;

mod lexer;
mod token;
mod position;
mod terminal;
mod error;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args[0]);
    }

    let mut lexer = lexer::Lexer::new(args[1].as_bytes(), 0);

    println!("  .globl main");
    println!("main:");

    println!("  mov ${}, %rax", lexer.token().as_ref().unwrap().token);

    loop {
        let tok = lexer.token();
        let tok = &tok.as_ref().unwrap().token;
        if *tok == token::Tok::EndOfFile {
            break;
        }
        if *tok == token::Tok::Plus {
            println!("  add ${}, %rax", lexer.token().as_ref().unwrap().token);
            continue;
        }
        if *tok == token::Tok::Minus {
            println!("  sub ${}, %rax", lexer.token().as_ref().unwrap().token);
            continue;
        }
    }
    println!("  ret");
}
