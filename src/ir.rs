use crate::{symbols, types};

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Constant(i64),
    Var(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Option<IrValue>),
    Unary {
        op: UnaryOperator,
        src: IrValue,
        dst: IrValue,
    },
    Binary {
        op: BinaryOperator,
        src1: IrValue,
        src2: IrValue,
        dst: IrValue,
    },
    Copy {
        src: IrValue,
        dst: IrValue,
    },
    GetAddress {
        src: IrValue,
        dst: IrValue,
    },
    Load {
        src_ptr: IrValue,
        dst: IrValue,
    },
    Store {
        src: IrValue,
        dst_ptr: IrValue,
    },
    AddrPtr {
        ptr: IrValue,
        index: IrValue,
        scale: i64,
        dst: IrValue,
    },
    CopyToOffset {
        src: IrValue,
        dst: String,
        offset: i64,
    },
    CopyFromOffset {
        src: String,
        offset: i64,
        dst: IrValue,
    },
    Jump(String),
    JumpIfZero(IrValue, String),
    JumpIfNotZero(IrValue, String),
    Label(String),
    FunCall {
        f: String,
        args: Vec<IrValue>,
        dst: Option<IrValue>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Function {
        name: String,
        global: bool,
        params: Vec<String>,
        body: Vec<Instruction>,
    },
    StaticVariable {
        name: String,
        t: types::Type,
    },
}

pub type Program = Vec<TopLevel>;
