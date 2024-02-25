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
}

pub type ExprWithPos = position::WithPos<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(ExprWithPos),
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
