#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    ConstChar(i8),
    ConstUChar(u8),
    ConstInt(i32),
    ConstLong(i64),
    ConstUInt(u32),
    ConstULong(u64),
    ConstDouble(f64),
}

pub fn const_to_dim(c: Type) -> i32 {
    match c {
        Type::ConstInt(i) => i,
        _ => panic!(),
    }
}
