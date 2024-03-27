use crate::{token, types};

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprInner {
    Binary {
        op: BinaryOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOperator,
        expr: Box<Expr>,
    },
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FunCall {
        funcname: String,
        args: Vec<Expr>,
    },
    Addr(Box<Expr>),
    Deref(Box<Expr>),
    Stmt(Box<Stmt>),
    Var(Obj),
    Num(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub e: ExprInner,
    pub ty: Option<types::Type>,
    pub token: token::Token,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Return(Expr),
    If {
        cond: Expr,
        then: Box<Stmt>,
        els: Box<Option<Stmt>>,
    },
    For {
        cond: Option<Expr>,
        then: Box<Stmt>,
        init: Box<Option<Stmt>>,
        inc: Box<Option<Expr>>,
    },
    Expr(Expr),
    Block(Vec<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    LocalVariable {
        name: String,
        ty: types::Type,
        offset: i64,
    },
    GlobalVariable {
        name: String,
        ty: types::Type,
        init_data: String,
    },
    Function {
        name: String,
        ty: types::Type,
        params: Vec<Obj>,
        body: Box<Stmt>,
        locals: Vec<Obj>,
        stack_size: i64,
    },
}
