pub type FileIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub name: &'static str,
    pub file_no: i32,
    pub contents: &'static [u8],
    pub display_name: String,
    pub line_delta: i32,
}

impl File {
    pub fn new(name: &'static str, file_no: i32, contents: &'static [u8]) -> Self {
        File {
            name,
            file_no,
            contents,
            display_name: String::new(),
            line_delta: 0,
        }
    }
}
