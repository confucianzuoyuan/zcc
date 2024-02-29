use crate::{position, types};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int {
        value: i64,
    },
    Binary {
        left: Box<ExprWithPos>,
        oper: OperatorWithPos,
        right: Box<ExprWithPos>,
    },
    Unary {
        oper: OperatorWithPos,
        expr: Box<ExprWithPos>,
    },
    Assign {
        lvalue: Box<ExprWithPos>,
        rvalue: Box<ExprWithPos>,
    },
    Var(VarObj),
    Addr(Box<ExprWithPos>),
    Deref(Box<ExprWithPos>),
    FunCall {
        funcname: String,
        args: Vec<ExprWithPos>,
    },
}

pub type ExprWithPos = position::WithPos<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub enum InnerTypedExpr {
    Int {
        value: i64,
    },
    Binary {
        left: Box<TypedExpr>,
        oper: Operator,
        right: Box<TypedExpr>,
    },
    Unary {
        oper: Operator,
        expr: Box<TypedExpr>,
    },
    Assign {
        lvalue: Box<TypedExpr>,
        rvalue: Box<TypedExpr>,
    },
    Var(VarObj),
    Addr(Box<TypedExpr>),
    Deref(Box<TypedExpr>),
    FunCall {
        funcname: String,
        args: Vec<TypedExpr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub expr: InnerTypedExpr,
    pub ty: types::Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<T> {
    Expr(T),
    Return(T),
    Block(Vec<StmtWithPos<T>>),
    Null,
    If {
        cond: Box<T>,
        then: Box<StmtWithPos<T>>,
        els: Box<Option<StmtWithPos<T>>>,
    },
    For {
        cond: Box<Option<T>>,
        then: Box<StmtWithPos<T>>,
        init: Box<StmtWithPos<T>>,
        inc: Box<Option<T>>,
    },
    While {
        cond: Box<T>,
        then: Box<StmtWithPos<T>>,
    },
}

pub type StmtWithPos<T> = position::WithPos<Stmt<T>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Divide,
    Minus,
    Plus,
    Times,
    LesserThan,
    LesserOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Equal,
    NotEqual,
}

pub type OperatorWithPos = position::WithPos<Operator>;

#[derive(Clone, Debug, PartialEq)]
pub struct VarObj {
    pub name: String,
    pub ty: types::Type,
    pub offset: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<T> {
    pub body: Box<StmtWithPos<T>>,
    pub stack_size: i64,
}
