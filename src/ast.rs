use crate::{constant, types};

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug)]
pub struct MemberDeclaration {
    pub member_name: String,
    pub member_type: types::Type,
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub tag: String,
    pub members: Vec<MemberDeclaration>,
}

#[derive(Debug)]
pub struct VariableDeclaration<InitType> {
    name: String,
    var_type: types::Type,
    init: Option<InitType>,
    storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub enum ForInit<InitType, ExpType> {
    InitDecl(VariableDeclaration<InitType>),
    InitExp(Option<ExpType>),
}

#[derive(Debug)]
pub enum Statement<InitType, ExpType> {
    Return(Option<ExpType>),
    Expression(ExpType),
    If {
        condition: ExpType,
        then_clause: Box<Statement<InitType, ExpType>>,
        else_clause: Box<Option<Statement<InitType, ExpType>>>,
    },
    Compound(Block<InitType, ExpType>),
    Break(String),
    Continue(String),
    While {
        condition: ExpType,
        body: Box<Statement<InitType, ExpType>>,
        id: String,
    },
    For {
        init: ForInit<InitType, ExpType>,
        condition: Option<ExpType>,
        post: Option<ExpType>,
        body: Box<Statement<InitType, ExpType>>,
        id: String,
    },
    Null,
}

#[derive(Debug)]
pub enum BlockItem<InitType, ExpType> {
    S(Statement<InitType, ExpType>),
    D(Declaration<InitType, ExpType>),
}

pub type Block<InitType, ExpType> = Vec<BlockItem<InitType, ExpType>>;

#[derive(Debug)]
pub struct FunctionDeclaration<InitType, ExpType> {
    name: String,
    fun_type: types::Type,
    params: Vec<String>,
    body: Option<Block<InitType, ExpType>>,
    storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub enum Declaration<InitType, ExpType> {
    FunDecl(FunctionDeclaration<InitType, ExpType>),
    VarDecl(VariableDeclaration<InitType>),
    StructDecl(StructDeclaration),
}

pub type Program<InitType, ExpType> = Vec<Declaration<InitType, ExpType>>;

#[derive(Debug)]
pub enum UntypedExp {
    Constant(constant::Type),
    Var(String),
    String(String),
    Cast {
        target_type: types::Type,
        e: Box<UntypedExp>,
    },
    Unary(UnaryOperator, Box<UntypedExp>),
    Binary(BinaryOperator, Box<UntypedExp>, Box<UntypedExp>),
    Assignment(Box<UntypedExp>, Box<UntypedExp>),
    Conditional {
        condition: Box<UntypedExp>,
        then_result: Box<UntypedExp>,
        else_result: Box<UntypedExp>,
    },
    FunCall {
        f: String,
        args: Vec<UntypedExp>,
    },
    Dereference(Box<UntypedExp>),
    AddrOf(Box<UntypedExp>),
    Subscript {
        ptr: Box<UntypedExp>,
        index: Box<UntypedExp>,
    },
    SizeOf(Box<UntypedExp>),
    SizeOfT(types::Type),
    Dot {
        strct: Box<UntypedExp>,
        member: String,
    },
    Arrow {
        strct: Box<UntypedExp>,
        member: String,
    },
}

#[derive(Debug)]
pub enum UntypedInitializer {
    SingleInit(UntypedExp),
    CompoundInit(Vec<UntypedExp>),
}

pub type UntypedProgram = Program<UntypedInitializer, UntypedExp>;

pub enum TypedInnerExp {
    Constant(constant::Type),
    Var(String),
    String(String),
    Cast {
        target_type: types::Type,
        e: TypedExp,
    },
    Unary(UnaryOperator, TypedExp),
    Binary(BinaryOperator, TypedExp, TypedExp),
    Assignment(TypedExp, TypedExp),
    Conditional {
        condition: TypedExp,
        then_result: TypedExp,
        else_result: TypedExp,
    },
    FunCall {
        f: String,
        args: Vec<TypedExp>,
    },
    Dereference(TypedExp),
    AddrOf(TypedExp),
    Subscript {
        ptr: TypedExp,
        index: TypedExp,
    },
    SizeOf(TypedExp),
    SizeOfT(types::Type),
    Dot {
        strct: TypedExp,
        member: String,
    },
    Arrow {
        strct: TypedExp,
        member: String,
    },
}

pub struct TypedExp {
    e: Box<TypedInnerExp>,
    t: types::Type,
}

pub enum TypedInitializer {
    SingleInit(TypedExp),
    CompoundInit(types::Type, Vec<TypedInitializer>),
}

pub type TypedProgram = Program<TypedInitializer, TypedExp>;
