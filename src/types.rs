#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Char,
    Pointer(Box<Type>),
    FunType {
        param_types: Vec<Type>,
        ret_type: Box<Type>,
    },
    Array {
        elem_type: Box<Type>,
        size: usize,
    },
}

pub fn pointer_to(base: Type) -> Type {
    Type::Pointer(Box::new(base))
}

pub fn get_type_size(t: Type) -> usize {
    match t {
        Type::Char => 1,
        Type::Int => 8,
        Type::Pointer(..) => 8,
        Type::Array { elem_type, size } => size * get_type_size(*elem_type),
        _ => panic!("type does not hava size: {:#?}", t),
    }
}
