use crate::constant;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Char,
    SChar,
    UChar,
    Int,
    Long,
    UInt,
    ULong,
    Double,
    Pointer(Box<Type>),
    Void,
    Array {
        elem_type: Box<Type>,
        size: i32,
    },
    FunType {
        param_types: Vec<Type>,
        ret_type: Box<Type>,
    },
    Structure(String),
}