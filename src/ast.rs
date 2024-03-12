use crate::types;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration<InitT> {
    pub name: String,
    pub var_type: types::Type,
    pub init: Option<InitT>,
}

pub type VariableDeclarations<InitT> = Vec<VariableDeclaration<InitT>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit<InitT, ExpT> {
    InitDecl(VariableDeclarations<InitT>),
    InitExp(Option<ExpT>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<InitT, ExpT> {
    Return(Option<ExpT>),
    Expression(ExpT),
    If {
        condition: ExpT,
        then_clause: Box<Statement<InitT, ExpT>>,
        else_clause: Box<Option<Statement<InitT, ExpT>>>,
    },
    Compound(Block<InitT, ExpT>),
    While {
        condition: ExpT,
        body: Box<Statement<InitT, ExpT>>,
        id: String,
    },
    For {
        init: ForInit<InitT, ExpT>,
        condition: Option<ExpT>,
        post: Option<ExpT>,
        body: Box<Statement<InitT, ExpT>>,
        id: String,
    },
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem<InitT, ExpT> {
    S(Statement<InitT, ExpT>),
    D(Declaration<InitT, ExpT>),
}

pub type Block<InitT, ExpT> = Vec<BlockItem<InitT, ExpT>>;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration<InitT, ExpT> {
    pub name: String,
    pub fun_type: types::Type,
    pub params: Vec<String>,
    pub body: Option<Block<InitT, ExpT>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration<InitT, ExpT> {
    FunDecl(FunctionDeclaration<InitT, ExpT>),
    VarDecl(VariableDeclarations<InitT>),
}

pub type Program<InitT, ExpT> = Vec<Declaration<InitT, ExpT>>;

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedExp {
    Constant(i64),
    Var(String),
    String(String),
    Unary(UnaryOperator, Box<UntypedExp>),
    Binary(BinaryOperator, Box<UntypedExp>, Box<UntypedExp>),
    Assignment(Box<UntypedExp>, Box<UntypedExp>),
    FunCall { f: String, args: Vec<UntypedExp> },
    Dereference(Box<UntypedExp>),
    AddrOf(Box<UntypedExp>),
    SizeOf(Box<UntypedExp>),
    SizeOfT(types::Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedInitializer {
    SingleInit(UntypedExp),
    CompoundInit(Vec<UntypedInitializer>),
}

pub type UntypedProgram = Program<UntypedInitializer, UntypedExp>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypedInnerExp {
    Constant(i64),
    Var(String),
    Unary(UnaryOperator, Box<TypedExp>),
    Binary(BinaryOperator, Box<TypedExp>, Box<TypedExp>),
    Assignment(Box<TypedExp>, Box<TypedExp>),
    FunCall { f: String, args: Vec<TypedExp> },
    Dereference(Box<TypedExp>),
    AddrOf(Box<TypedExp>),
    SizeOf(Box<TypedExp>),
    SizeOfT(types::Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExp {
    pub e: TypedInnerExp,
    pub t: types::Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedInitializer {
    SingleInit(TypedExp),
    CompoundInit(types::Type, Vec<TypedInitializer>),
}

pub type TypedProgram = Program<TypedInitializer, TypedExp>;
