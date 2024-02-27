use crate::position;

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
}

pub type ExprWithPos = position::WithPos<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(ExprWithPos),
    Return(ExprWithPos),
    Block(Vec<StmtWithPos>),
    Null,
    If {
        cond: Box<ExprWithPos>,
        then: Box<StmtWithPos>,
        els: Box<Option<StmtWithPos>>,
    },
}

pub type StmtWithPos = position::WithPos<Stmt>;

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
    pub offset: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub body: Box<StmtWithPos>,
    pub stack_size: i64,
}
