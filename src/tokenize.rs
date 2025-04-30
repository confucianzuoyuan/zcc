pub struct Tokenizer {
    /// Input file
    pub current_file: File,
    /// A list of all input files.
    pub input_files: Vec<File>,
    /// True if the current position is at the beginning of a line
    pub at_bol: bool,
    /// True if the current position follows a space character
    pub has_space: bool,
}

pub struct File {
    pub name: String,
    pub file_no: i32,
    pub contents: String,
    pub display_name: String,
    pub line_delta: i32,
}
