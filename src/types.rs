#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Pointer(Box<Type>),
    FunType {
        param_types: Vec<Type>,
        ret_type: Box<Type>,
    },
}

pub fn pointer_to(base: Type) -> Type {
    Type::Pointer(Box::new(base))
}
