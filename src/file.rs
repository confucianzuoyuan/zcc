pub type FileIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub name: String,
    pub file_no: i32,
    pub contents: &'static [u8],
    pub display_name: String,
    pub line_delta: i32,
}
