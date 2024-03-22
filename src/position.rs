use std::fmt::Display;

use crate::{symbols, terminal};

#[derive(Clone, Debug, PartialEq)]
pub struct Pos {
    pub loc: String,  // Token location
    pub len: usize,   // Token length
    pub line_no: u32, // Line number
}
