use crate::position;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    Eof,
    Msg(String),
    UnknownToken { pos: position::Pos, start: char },
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
