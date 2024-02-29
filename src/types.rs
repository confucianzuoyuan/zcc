use crate::{ast, position};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    IntType,
    PointerType { base: Box<Type> },
}

pub fn pointer_to(base: Type) -> Type {
    Type::PointerType {
        base: Box::new(base),
    }
}

pub fn convert_expr_to_typed_expr(expr: ast::ExprWithPos) -> ast::TypedExpr {
    match expr.node {
        ast::Expr::Int { value } => ast::TypedExpr {
            expr: ast::InnerTypedExpr::Int { value },
            ty: Type::IntType,
        },
        ast::Expr::Var(v) => ast::TypedExpr {
            expr: ast::InnerTypedExpr::Var(v.clone()),
            ty: v.ty,
        },
        ast::Expr::Binary {
            left,
            oper:
                position::WithPos {
                    node: ast::Operator::Plus,
                    ..
                },
            right,
        } => {
            let left = convert_expr_to_typed_expr(*left);
            let right = convert_expr_to_typed_expr(*right);
            let oper = ast::Operator::Plus;

            match (left.ty.clone(), right.ty.clone()) {
                // num + num
                (Type::IntType, Type::IntType) => ast::TypedExpr {
                    expr: ast::InnerTypedExpr::Binary {
                        left: Box::new(left),
                        oper,
                        right: Box::new(right),
                    },
                    ty: Type::IntType,
                },
                // ptr + num
                (Type::PointerType { .. }, Type::IntType) => {
                    let right = ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(right),
                            oper: ast::Operator::Times,
                            right: Box::new(ast::TypedExpr {
                                expr: ast::InnerTypedExpr::Int { value: 8 },
                                ty: Type::IntType,
                            }),
                        },
                        ty: left.ty.clone(),
                    };
                    ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper: ast::Operator::Plus,
                            right: Box::new(right),
                        },
                        ty: left.ty,
                    }
                }
                // num + ptr
                (Type::IntType, Type::PointerType { .. }) => {
                    let left = ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper: ast::Operator::Times,
                            right: Box::new(ast::TypedExpr {
                                expr: ast::InnerTypedExpr::Int { value: 8 },
                                ty: Type::IntType,
                            }),
                        },
                        ty: left.ty,
                    };
                    ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper: ast::Operator::Plus,
                            right: Box::new(right.clone()),
                        },
                        ty: right.ty,
                    }
                }
                (Type::PointerType { .. }, Type::PointerType { .. }) => {
                    panic!("cannot add two pointers")
                }
            }
        }
        ast::Expr::Binary {
            left,
            oper:
                position::WithPos {
                    node: ast::Operator::Minus,
                    ..
                },
            right,
        } => {
            let left = convert_expr_to_typed_expr(*left);
            let right = convert_expr_to_typed_expr(*right);
            let oper = ast::Operator::Minus;

            match (left.ty.clone(), right.ty.clone()) {
                // num - num
                (Type::IntType, Type::IntType) => ast::TypedExpr {
                    expr: ast::InnerTypedExpr::Binary {
                        left: Box::new(left),
                        oper,
                        right: Box::new(right),
                    },
                    ty: Type::IntType,
                },
                // ptr - num
                (Type::PointerType { .. }, Type::IntType) => {
                    let right = ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(right),
                            oper: ast::Operator::Times,
                            right: Box::new(ast::TypedExpr {
                                expr: ast::InnerTypedExpr::Int { value: 8 },
                                ty: Type::IntType,
                            }),
                        },
                        ty: left.ty.clone(),
                    };
                    ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper,
                            right: Box::new(right),
                        },
                        ty: left.ty,
                    }
                }
                // ptr - ptr
                (Type::PointerType { .. }, Type::PointerType { .. }) => {
                    let left = ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper,
                            right: Box::new(right),
                        },
                        ty: left.ty,
                    };
                    ast::TypedExpr {
                        expr: ast::InnerTypedExpr::Binary {
                            left: Box::new(left.clone()),
                            oper: ast::Operator::Divide,
                            right: Box::new(ast::TypedExpr {
                                expr: ast::InnerTypedExpr::Int { value: 8 },
                                ty: Type::IntType,
                            }),
                        },
                        ty: Type::IntType,
                    }
                }
                // wrong: num - ptr
                (Type::IntType, Type::PointerType { .. }) => panic!("cannot add two pointers"),
            }
        }
        ast::Expr::Binary { left, oper, right } => {
            let left = convert_expr_to_typed_expr(*left);
            let right = convert_expr_to_typed_expr(*right);
            let oper = oper.node;
            ast::TypedExpr {
                expr: ast::InnerTypedExpr::Binary {
                    left: Box::new(left.clone()),
                    oper,
                    right: Box::new(right),
                },
                ty: left.ty,
            }
        }
        ast::Expr::Unary { oper, expr } => {
            let expr = convert_expr_to_typed_expr(*expr);
            let oper = oper.node;
            ast::TypedExpr {
                expr: ast::InnerTypedExpr::Unary {
                    oper,
                    expr: Box::new(expr.clone()),
                },
                ty: expr.ty,
            }
        }
        ast::Expr::Assign { lvalue, rvalue } => {
            let lvalue = convert_expr_to_typed_expr(*lvalue);
            let rvalue = convert_expr_to_typed_expr(*rvalue);
            ast::TypedExpr {
                expr: ast::InnerTypedExpr::Assign {
                    lvalue: Box::new(lvalue),
                    rvalue: Box::new(rvalue.clone()),
                },
                ty: rvalue.ty,
            }
        }
        ast::Expr::Addr(e) => {
            let e = convert_expr_to_typed_expr(*e);
            ast::TypedExpr {
                expr: ast::InnerTypedExpr::Addr(Box::new(e.clone())),
                ty: Type::PointerType {
                    base: Box::new(e.ty),
                },
            }
        }
        ast::Expr::Deref(e) => {
            let e = convert_expr_to_typed_expr(*e);
            let ty = match e.clone().ty {
                Type::PointerType { base, .. } => *base,
                Type::IntType => panic!("invalid pointer dereference"),
            };
            ast::TypedExpr {
                expr: ast::InnerTypedExpr::Deref(Box::new(e)),
                ty,
            }
        }
        ast::Expr::FunCall { funcname } => ast::TypedExpr {
            expr: ast::InnerTypedExpr::FunCall { funcname },
            ty: Type::IntType,
        },
    }
}

pub fn convert_stmt_to_typed_stmt(
    stmt: ast::StmtWithPos<ast::ExprWithPos>,
) -> ast::StmtWithPos<ast::TypedExpr> {
    match stmt.node {
        ast::Stmt::Block(block_items) => {
            let mut result_block_items = vec![];
            for item in block_items {
                result_block_items.push(convert_stmt_to_typed_stmt(item));
            }
            ast::StmtWithPos {
                node: ast::Stmt::Block(result_block_items),
                pos: stmt.pos,
            }
        }
        ast::Stmt::Expr(e) => {
            let e = convert_expr_to_typed_expr(e);
            ast::StmtWithPos {
                node: ast::Stmt::Expr(e),
                pos: stmt.pos,
            }
        }
        ast::Stmt::If { cond, then, els } => {
            let cond = convert_expr_to_typed_expr(*cond);
            let then = convert_stmt_to_typed_stmt(*then);
            let els = match *els {
                Some(_els) => Some(convert_stmt_to_typed_stmt(_els)),
                None => None,
            };
            ast::StmtWithPos {
                node: ast::Stmt::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    els: Box::new(els),
                },
                pos: stmt.pos,
            }
        }
        ast::Stmt::For {
            cond,
            then,
            init,
            inc,
        } => {
            let cond = match *cond {
                Some(_cond) => Some(convert_expr_to_typed_expr(_cond)),
                None => None,
            };
            let then = convert_stmt_to_typed_stmt(*then);
            let init = convert_stmt_to_typed_stmt(*init);
            let inc = match *inc {
                Some(_inc) => Some(convert_expr_to_typed_expr(_inc)),
                None => None,
            };
            ast::StmtWithPos {
                node: ast::Stmt::For {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    init: Box::new(init),
                    inc: Box::new(inc),
                },
                pos: stmt.pos,
            }
        }
        ast::Stmt::While { cond, then } => {
            let cond = convert_expr_to_typed_expr(*cond);
            let then = convert_stmt_to_typed_stmt(*then);
            ast::StmtWithPos {
                node: ast::Stmt::While {
                    cond: Box::new(cond),
                    then: Box::new(then),
                },
                pos: stmt.pos,
            }
        }
        ast::Stmt::Return(e) => {
            let e = convert_expr_to_typed_expr(e);
            ast::StmtWithPos {
                node: ast::Stmt::Return(e),
                pos: stmt.pos,
            }
        }
        ast::Stmt::Null => ast::StmtWithPos {
            node: ast::Stmt::Null,
            pos: stmt.pos,
        },
    }
}

pub fn convert_function_to_typed_function(
    f: ast::Function<ast::ExprWithPos>,
) -> ast::Function<ast::TypedExpr> {
    ast::Function {
        body: Box::new(convert_stmt_to_typed_stmt(*f.body)),
        stack_size: f.stack_size,
    }
}
