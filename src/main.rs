mod lexer;
mod token;
mod ast;
mod parser;
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

    let lexer = lexer::Lexer::new(input_path);
    let mut parser = parser::Parser::new(lexer);
    parser.parse();
}
