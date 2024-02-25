use std::{fs::File, io::{Read, Seek}};

use crate::{position, symbol, terminal, token};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    Eof,
    Msg(String),
    UnknownToken {
        pos: position::Pos,
        start: char,
    },
    UnexpectedToken {
        expected: String,
        pos: position::Pos,
        unexpected: token::Tok,
    },
    Multi(Vec<Error>),
}

impl Error {
    pub fn show(
        &self,
        symbols: &symbol::Symbols,
        terminal: &terminal::Terminal,
    ) -> std::io::Result<()> {
        if let Error::Multi(ref errors) = *self {
            for error in errors.iter().rev() {
                error.show(symbols, terminal)?;
            }
            return Ok(());
        }
        eprint!(
            "{}{}error: {}",
            terminal.bold(),
            terminal.red(),
            terminal.reset_color()
        );
        match *self {
            Error::Multi(_) => unreachable!(),
            Error::UnexpectedToken {
                ref expected,
                pos,
                ref unexpected,
            } => {
                eprintln!(
                    "Unexpected token {}, expecting {}{}",
                    unexpected,
                    expected,
                    terminal.end_bold()
                );
                pos.show(symbols, terminal);
            }
            Error::UnknownToken { pos, ref start } => {
                eprintln!(
                    "Unexpected start of token `{}`{}",
                    start,
                    terminal.end_bold()
                );
                pos.show(symbols, terminal);
            }
            Error::Eof => eprintln!("end of file"),
            Error::Msg(ref string) => eprintln!("{}", string),
        }
        eprintln!();

        Ok(())
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Msg(value.to_string())
    }
}

impl<'a> From<&'a Error> for Error {
    fn from(value: &'a Error) -> Self {
        value.clone()
    }
}

pub fn num_text_size(num: i64) -> usize {
    if num == 0 {
        return 1;
    }
    1 + (num as f64).log10().floor() as usize
}
