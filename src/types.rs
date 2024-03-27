use crate::{ast, token};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    CHAR,
    INT,
    PTR {
        base: Box<Type>,
        size: usize,
    },
    FUNC {
        name: Option<token::Token>,
        return_ty: Box<Type>,
        params: Vec<Type>,
    },
    ARRAY {
        base: Box<Type>,
        size: usize,
        array_len: usize,
    },
    DUMMY, // 占位符，无意义
}

pub fn get_size(t: Type) -> usize {
    match t {
        Type::CHAR => 1,
        Type::INT => 8,
        Type::PTR { size, .. } => size,
        Type::ARRAY { size, .. } => size,
        _ => panic!(),
    }
}

pub fn is_integer(t: Type) -> bool {
    match t {
        Type::CHAR | Type::INT => true,
        _ => false,
    }
}

pub fn pointer_to(base: Type) -> Type {
    Type::PTR {
        base: Box::new(base),
        size: 8,
    }
}

pub fn array_of(base: Type, len: usize) -> Type {
    Type::ARRAY {
        base: Box::new(base.clone()),
        size: get_size(base) * len,
        array_len: len,
    }
}
