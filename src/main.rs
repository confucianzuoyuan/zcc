#![allow(unused_imports)]
#![allow(dead_code)]

use std::rc::Rc;

mod error;
mod lexer;
mod position;
mod symbols;
mod terminal;
mod token;

fn main() {
    let strings = Rc::new(symbols::Strings::new());
    let mut symbols = symbols::Symbols::new(Rc::clone(&strings));
    if let Err(error) = drive(strings, &mut symbols) {
        let terminal = terminal::Terminal::new();
        if let Err(error) = error.show(&symbols, &terminal) {
            eprint!("Error printing errors: {}", error);
        }
    }
}

fn drive(
    strings: Rc<symbols::Strings>,
    symbols: &mut symbols::Symbols<()>,
) -> Result<(), error::Error> {
    let mut args = std::env::args();
    args.next();
    if let Some(filename) = args.next() {
        let file = std::io::BufReader::new(std::fs::File::open(&filename)?);
        let file_symbol = symbols.symbol(&filename);
        let mut lexer = lexer::Lexer::new(file, file_symbol);
        let mut token;
        loop {
            token = lexer.token()?;
            println!("{:?}", token);
            if token.token == token::Tok::EndOfFile {
                break;
            }
        }
    }
    Ok(())
}
