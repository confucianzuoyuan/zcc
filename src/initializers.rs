#[derive(Debug, Clone, PartialEq)]
pub enum StaticInit {
    CharInit(i8),
    UCharInit(u8),
    IntInit(i32),
    LongInit(i64),
    UIntInit(u32),
    ULongInit(u64),
    PointerInit(String),
}
