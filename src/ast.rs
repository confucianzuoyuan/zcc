use crate::position;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int {
        value: i64,
    },
    Oper {
        left: Box<ExprWithPos>,
        oper: OperatorWithPos,
        right: Box<ExprWithPos>,
    },
}

pub type ExprWithPos = position::WithPos<Expr>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Divide,
    Minus,
    Plus,
    Times,
}

pub type OperatorWithPos = position::WithPos<Operator>;
