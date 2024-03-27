#[derive(Clone, Debug)]
pub struct Error {
    pub line_number: usize,
    pub location: usize,
    pub message: String,
}

pub struct ErrorHandler {
    pub filename: String,
    pub code: Vec<u8>,
}

impl ErrorHandler {
    pub fn new(filename: String, code: Vec<u8>) -> Self {
        Self { filename, code }
    }

    /// Reports an error message in the following format and exit.
    ///
    /// foo.c:10: x = y + 1;
    ///               ^ <error message here>
    pub fn show(&self, e: Error) {
        // Find a line containing `loc`.
        let mut line = e.location;
        while line > 0 && self.code[line - 1] != b'\n' {
            line -= 1;
        }

        let mut end = e.location;
        while self.code[end] != b'\n' {
            end += 1;
        }

        // Print out the line
        let line_msg = format!("{}:{}: ", self.filename, e.line_number);
        let indent = line_msg.len();
        eprint!("{}", line_msg);
        eprintln!("{}", String::from_utf8(self.code[line..end].to_vec()).unwrap());

        // Show the error message.
        let pos = e.location - line + indent;

        eprint!("{}", " ".repeat(pos));
        eprintln!("^ {}", e.message);
        std::process::exit(1);
    }
}
