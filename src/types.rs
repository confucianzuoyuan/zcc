#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    CHAR,
    INT,
    PTR {
        base: Box<Type>,
        size: u64,
    },
    FUNC {
        return_ty: Box<Type>,
    },
    ARRAY {
        base: Box<Type>,
        size: u64,
        array_len: u64,
    },
}

pub fn get_size(t: Type) -> u64 {
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

pub fn func_type(return_ty: Type) -> Type {
    Type::FUNC {
        return_ty: Box::new(return_ty),
    }
}

pub fn array_of(base: Type, len: u64) -> Type {
    Type::ARRAY {
        base: Box::new(base.clone()),
        size: get_size(base) * len,
        array_len: len,
    }
}
