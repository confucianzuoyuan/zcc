use std::io::BufRead;

mod ast;
mod constant;
mod error;
mod lexer;
mod parser;
mod token;
mod types;

fn usage(status: i32) {
    eprintln!("zcc [ -o <path> ] <file>");
    std::process::exit(status);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut i = 1;
    let mut opt_o = String::new();
    let mut input_path = String::new();
    while i < args.len() {
        if args[i] == "--help" {
            usage(0);
        }

        if args[i] == "-o" {
            i += 1;
            if i == args.len() {
                usage(1);
            }
            opt_o = args[i].clone();
            i += 1;
            continue;
        }

        if args[i].starts_with("-o") {
            opt_o = args[i][2..].to_string();
            i += 1;
            continue;
        }

        if args[i].chars().nth(0).unwrap() == '-' && args[i].chars().nth(1).is_some() {
            eprintln!("unknown argument: {}", args[i]);
            std::process::exit(1);
        }

        input_path = args[i].clone();
        i += 1;
    }

    if input_path.is_empty() {
        eprintln!("no input files");
    }

    let source_code = read_file(input_path.clone());
    let error_handler = error::ErrorHandler::new(input_path, source_code.clone());
    let mut lexer = lexer::Lexer::new(source_code);
    let tokens = lexer.tokenize();
    match tokens {
        Ok(_tokens) => {
            let mut parser = parser::Parser::new(_tokens);
            let ast = parser.parse();
            match ast {
                Ok(_ast) => println!("{:#?}", _ast),
                Err(e) => error_handler.show(e),
            }
        }
        Err(e) => error_handler.show(e),
    }
}

/// Returns the contents of a given file.
fn read_file(input_path: String) -> Vec<u8> {
    let mut source_code = vec![];
    if input_path == "-" {
        let stdin = std::io::stdin();
        for line in stdin.lock().lines() {
            source_code.append(&mut line.unwrap().as_bytes().to_vec());
            source_code.push(b'\n');
        }
    } else {
        source_code = std::fs::read_to_string(input_path.clone())
            .expect("no such file")
            .as_bytes()
            .to_vec();
        if !source_code.ends_with(&[b'\n']) {
            source_code.push(b'\n');
        }
    }

    source_code
}
