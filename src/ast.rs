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
pub enum Expr {
    Binary {
        op: BinaryOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary(UnaryOperator, Box<Expr>),
    FunCall {
        funcname: String,
        args: Vec<Expr>,
    },
    Addr(Box<Expr>),
    Deref(Box<Expr>),
    Stmt(Box<Expr>),
    Var(String),
    Num(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Return(Expr),
    If {
        cond: Expr,
        then: Box<Stmt>,
        els: Box<Stmt>,
    },
    For {
        cond: Expr,
        then: Box<Stmt>,
        els: Box<Stmt>,
        init: Box<Stmt>,
        inc: Box<Stmt>,
    },
    Expr(Expr),
}
