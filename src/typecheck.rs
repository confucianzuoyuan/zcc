use crate::types::get_type_size;
use crate::{ast, symbols, types, unique_ids};

fn validate_type(typ: types::Type) {
    match typ {
        types::Type::Pointer(t) => validate_type(*t),
        types::Type::FunType {
            param_types,
            ret_type,
        } => {
            for param_type in param_types {
                validate_type(param_type);
            }
            validate_type(*ret_type);
        }
        types::Type::Int | types::Type::Char | types::Type::Array { .. } => (),
    }
}

fn typecheck_var(v: String) -> ast::TypedExp {
    let v_type = symbols::get_opt(v.clone());
    match v_type {
        Some(entry) => {
            let e = ast::TypedInnerExp::Var(v);
            match entry.t {
                types::Type::FunType { .. } => panic!("Tried to use function name as variable"),
                _ => ast::TypedExp { e, t: entry.t },
            }
        }
        None => panic!("var not in symbol table"),
    }
}

fn typecheck_exp(e: ast::UntypedExp) -> ast::TypedExp {
    match e {
        ast::UntypedExp::Var(v) => typecheck_var(v),
        ast::UntypedExp::Constant(c) => ast::TypedExp {
            e: ast::TypedInnerExp::Constant(c),
            t: types::Type::Int,
        },
        ast::UntypedExp::String(string) => {
            let string_label = unique_ids::new_unique_name();
            let string_bytes = string.as_bytes();
            let mut string_bytes_static_init = vec![];
            for b in string_bytes {
                string_bytes_static_init.push(symbols::StaticInit::CharInit(*b));
            }
            symbols::add_static_var(
                string_label.clone(),
                types::Type::Array {
                    elem_type: Box::new(types::Type::Char),
                    size: string.len() + 1,
                },
                true,
                symbols::InitialValue::Initial(string_bytes_static_init),
            );
            ast::TypedExp {
                e: ast::TypedInnerExp::Var(string_label),
                t: types::Type::Array {
                    elem_type: Box::new(types::Type::Char),
                    size: string.len() + 1,
                },
            }
        }
        ast::UntypedExp::Unary(ast::UnaryOperator::Negate, inner) => {
            let typed_inner = typecheck_exp(*inner);
            ast::TypedExp {
                e: ast::TypedInnerExp::Unary(
                    ast::UnaryOperator::Negate,
                    Box::new(typed_inner.clone()),
                ),
                t: typed_inner.t,
            }
        }
        ast::UntypedExp::FunCall { f, args } => {
            let mut typed_args = vec![];
            for arg in args {
                typed_args.push(typecheck_exp(arg));
            }
            ast::TypedExp {
                e: ast::TypedInnerExp::FunCall {
                    f,
                    args: typed_args,
                },
                t: types::Type::Int,
            }
        }
        ast::UntypedExp::Binary(
            op @ (ast::BinaryOperator::GreaterOrEqual
            | ast::BinaryOperator::Equal
            | ast::BinaryOperator::NotEqual
            | ast::BinaryOperator::GreaterThan
            | ast::BinaryOperator::LessOrEqual
            | ast::BinaryOperator::LessThan),
            left,
            right,
        ) => {
            let typed_left = typecheck_exp(*left);
            let typed_right = typecheck_exp(*right);
            ast::TypedExp {
                e: ast::TypedInnerExp::Binary(op, Box::new(typed_left), Box::new(typed_right)),
                t: types::Type::Int,
            }
        }
        ast::UntypedExp::Binary(
            op @ (ast::BinaryOperator::Divide | ast::BinaryOperator::Multiply),
            left,
            right,
        ) => {
            let typed_left = typecheck_exp(*left);
            let typed_right = typecheck_exp(*right);
            let ty = typed_left.t.clone();
            ast::TypedExp {
                e: ast::TypedInnerExp::Binary(op, Box::new(typed_left), Box::new(typed_right)),
                t: ty,
            }
        }
        // In C, `+` operator is overloaded to perform the pointer arithmetic.
        // If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
        // so that p+n points to the location n elements (not bytes) ahead of p.
        // In other words, we need to scale an integer value before adding to a
        // pointer value. This function takes care of the scaling.
        ast::UntypedExp::Binary(ast::BinaryOperator::Add, left, right) => {
            let typed_left = typecheck_exp(*left);
            let typed_right = typecheck_exp(*right);

            match (typed_left.t.clone(), typed_right.t.clone()) {
                // num + num
                (types::Type::Int, types::Type::Int) => ast::TypedExp {
                    e: ast::TypedInnerExp::Binary(
                        ast::BinaryOperator::Add,
                        Box::new(typed_left),
                        Box::new(typed_right),
                    ),
                    t: types::Type::Int,
                },
                // ptr + num
                (types::Type::Pointer(..), types::Type::Int) => {
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(get_type_size(typed_left.t.clone()) as i64),
                        t: types::Type::Int,
                    };
                    let rhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Multiply,
                            Box::new(typed_right),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Add,
                            Box::new(typed_left.clone()),
                            Box::new(rhs),
                        ),
                        t: typed_left.t,
                    }
                }
                // num + ptr
                (types::Type::Int, types::Type::Pointer(base)) => {
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(get_type_size(*base) as i64),
                        t: types::Type::Int,
                    };
                    let lhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Multiply,
                            Box::new(typed_left),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Add,
                            Box::new(lhs),
                            Box::new(typed_right.clone()),
                        ),
                        t: typed_right.t,
                    }
                }
                // array + num
                // int x[3]; `x + 1`的求值结果是指针类型，指向int的指针
                // int x[2][3]; `x + 1`的求值结果是指针类型，指向int [3]的指针
                (types::Type::Array { elem_type, size: _ }, types::Type::Int) => {
                    let lhs = ast::TypedExp {
                        e: ast::TypedInnerExp::AddrOf(Box::new(typed_left)),
                        t: types::pointer_to(*elem_type.clone()),
                    };
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(get_type_size(*elem_type.clone()) as i64),
                        t: types::Type::Int,
                    };
                    let rhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Multiply,
                            Box::new(typed_right),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Add,
                            Box::new(lhs.clone()),
                            Box::new(rhs),
                        ),
                        t: lhs.t,
                    }
                }
                // num + array
                // int 2[x]
                (types::Type::Int, types::Type::Array { elem_type, size: _ }) => {
                    let rhs = ast::TypedExp {
                        e: ast::TypedInnerExp::AddrOf(Box::new(typed_right)),
                        t: types::pointer_to(*elem_type.clone()),
                    };
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(get_type_size(*elem_type.clone()) as i64),
                        t: types::Type::Int,
                    };
                    let lhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Multiply,
                            Box::new(typed_left),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Add,
                            Box::new(rhs.clone()),
                            Box::new(lhs),
                        ),
                        t: rhs.t,
                    }
                }
                // ptr + ptr
                _ => panic!("invalid operands"),
            }
        }
        ast::UntypedExp::Binary(ast::BinaryOperator::Subtract, left, right) => {
            let typed_left = typecheck_exp(*left);
            let typed_right = typecheck_exp(*right);

            match (typed_left.t.clone(), typed_right.t.clone()) {
                // num - num
                (types::Type::Int | types::Type::Char, types::Type::Int | types::Type::Char) => {
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Subtract,
                            Box::new(typed_left),
                            Box::new(typed_right),
                        ),
                        t: types::Type::Int,
                    }
                }
                // ptr - num
                (types::Type::Pointer(..), types::Type::Int) => {
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(8),
                        t: types::Type::Int,
                    };
                    let rhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Multiply,
                            Box::new(typed_right),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Subtract,
                            Box::new(typed_left.clone()),
                            Box::new(rhs),
                        ),
                        t: typed_left.t,
                    }
                }
                // ptr - ptr
                (types::Type::Pointer(..), types::Type::Pointer(..)) => {
                    let typed_num_eight = ast::TypedExp {
                        e: ast::TypedInnerExp::Constant(8),
                        t: types::Type::Int,
                    };
                    let lhs = ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Subtract,
                            Box::new(typed_left),
                            Box::new(typed_right),
                        ),
                        t: types::Type::Int,
                    };
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Binary(
                            ast::BinaryOperator::Divide,
                            Box::new(lhs),
                            Box::new(typed_num_eight),
                        ),
                        t: types::Type::Int,
                    }
                }
                // num - ptr
                _ => panic!("invalid operands"),
            }
        }
        ast::UntypedExp::AddrOf(e) => {
            let typed_e = typecheck_exp(*e);
            match typed_e.t.clone() {
                types::Type::Array { .. } => {
                    let ty = types::pointer_to(typed_e.t.clone());
                    ast::TypedExp {
                        e: ast::TypedInnerExp::AddrOf(Box::new(typed_e)),
                        t: ty,
                    }
                }
                _ => {
                    let ty = types::pointer_to(typed_e.t.clone());
                    ast::TypedExp {
                        e: ast::TypedInnerExp::AddrOf(Box::new(typed_e)),
                        t: ty,
                    }
                }
            }
        }
        ast::UntypedExp::Dereference(e) => {
            let typed_e = typecheck_exp(*e);
            match typed_e.t.clone() {
                types::Type::Pointer(base) => {
                    let ty = *base;
                    ast::TypedExp {
                        e: ast::TypedInnerExp::Dereference(Box::new(typed_e)),
                        t: ty,
                    }
                }
                // int x[2][3]; `*x`的类型是`int [3]`
                types::Type::Array { elem_type, size: _ } => ast::TypedExp {
                    e: ast::TypedInnerExp::Dereference(Box::new(typed_e)),
                    t: *elem_type,
                },
                _ => panic!("invalid pointer dereference"),
            }
        }
        ast::UntypedExp::Assignment(lhs, rhs) => {
            let typed_lhs = typecheck_exp(*lhs);
            let typed_rhs = typecheck_exp(*rhs);
            let ty = typed_rhs.t.clone();
            ast::TypedExp {
                e: ast::TypedInnerExp::Assignment(Box::new(typed_lhs), Box::new(typed_rhs)),
                t: ty,
            }
        }
        ast::UntypedExp::SizeOf(e) => {
            let typed_e = typecheck_exp(*e);
            ast::TypedExp {
                e: ast::TypedInnerExp::SizeOf(Box::new(typed_e)),
                t: types::Type::Int,
            }
        }
        ast::UntypedExp::SizeOfT(..) => panic!("not support sizeof Type now"),
    }
}

fn typecheck_statement(
    stmt: ast::Statement<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::Statement<ast::TypedInitializer, ast::TypedExp> {
    match stmt {
        ast::Statement::Return(e) => match e {
            Some(_e) => {
                let typed_e = typecheck_exp(_e);
                ast::Statement::Return(Some(typed_e))
            }
            None => ast::Statement::Return(None),
        },
        ast::Statement::Expression(e) => ast::Statement::Expression(typecheck_exp(e)),
        ast::Statement::If {
            condition,
            then_clause,
            else_clause,
        } => {
            let typed_condition = typecheck_exp(condition);
            let typed_then = typecheck_statement(*then_clause);
            let typed_else = match *else_clause {
                Some(_else) => Some(typecheck_statement(_else)),
                None => None,
            };
            ast::Statement::If {
                condition: typed_condition,
                then_clause: Box::new(typed_then),
                else_clause: Box::new(typed_else),
            }
        }
        ast::Statement::While { condition, body } => {
            let typed_condition = typecheck_exp(condition);
            let typed_body = typecheck_statement(*body);
            ast::Statement::While {
                condition: typed_condition,
                body: Box::new(typed_body),
            }
        }
        ast::Statement::For {
            init,
            condition,
            post,
            body,
        } => {
            let typed_init = typecheck_for_init(init);
            let typed_condition = match condition {
                Some(_cond) => Some(typecheck_exp(_cond)),
                None => None,
            };
            let typed_post = match post {
                Some(_post) => Some(typecheck_exp(_post)),
                None => None,
            };
            let typed_body = typecheck_statement(*body);
            ast::Statement::For {
                init: typed_init,
                condition: typed_condition,
                post: typed_post,
                body: Box::new(typed_body),
            }
        }
        ast::Statement::Compound(block_items) => {
            let mut typed_block_items = vec![];
            for bi in block_items {
                let typed_bi = typecheck_block_item(bi);
                typed_block_items.push(typed_bi);
            }
            ast::Statement::Compound(typed_block_items)
        }
        ast::Statement::Null => ast::Statement::Null,
    }
}

fn typecheck_block_item(
    block_item: ast::BlockItem<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::BlockItem<ast::TypedInitializer, ast::TypedExp> {
    match block_item {
        ast::BlockItem::S(s) => ast::BlockItem::S(typecheck_statement(s)),
        ast::BlockItem::D(d) => ast::BlockItem::D(typecheck_local_decl(d)),
    }
}

fn typecheck_local_decl(
    decl: ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::Declaration<ast::TypedInitializer, ast::TypedExp> {
    match decl {
        ast::Declaration::VarDecl(vd) => ast::Declaration::VarDecl(typecheck_local_var_decl(vd)),
        ast::Declaration::FunDecl(fd) => ast::Declaration::FunDecl(typecheck_fn_decl(fd)),
    }
}

fn get_init_type(init: ast::TypedInitializer) -> types::Type {
    match init {
        ast::TypedInitializer::SingleInit(typed_exp) => typed_exp.t,
        ast::TypedInitializer::CompoundInit(..) => panic!(),
    }
}

fn typecheck_local_var_decl(
    vd: ast::VariableDeclarations<ast::UntypedInitializer>,
) -> ast::VariableDeclarations<ast::TypedInitializer> {
    let mut typed_var_decls = vec![];
    for var_decl in vd {
        match var_decl.init {
            Some(init) => {
                let typed_init = typecheck_init(init);
                let var_type = match get_init_type(typed_init.clone()) {
                    types::Type::Array { elem_type, .. } => types::Type::Pointer(elem_type),
                    _ => var_decl.var_type.clone(),
                };
                symbols::add_automatic_var(var_decl.name.clone(), var_type.clone());
                typed_var_decls.push(ast::VariableDeclaration {
                    name: var_decl.name,
                    var_type,
                    init: Some(typed_init),
                })
            }
            None => {
                symbols::add_automatic_var(var_decl.name.clone(), var_decl.var_type.clone());
                typed_var_decls.push(ast::VariableDeclaration {
                    name: var_decl.name,
                    var_type: var_decl.var_type,
                    init: None,
                });
            }
        }
    }
    typed_var_decls
}

fn typecheck_file_scope_var_decl(
    vd: ast::VariableDeclarations<ast::UntypedInitializer>,
) -> ast::VariableDeclarations<ast::TypedInitializer> {
    let mut typed_var_decls = vec![];
    for var_decl in vd {
        match var_decl.init {
            Some(init) => {
                let typed_init = typecheck_init(init);
                let var_type = match get_init_type(typed_init.clone()) {
                    types::Type::Array { elem_type, .. } => types::Type::Pointer(elem_type),
                    _ => var_decl.var_type.clone(),
                };
                symbols::add_static_var(
                    var_decl.name.clone(),
                    var_type.clone(),
                    true,
                    symbols::InitialValue::Initial(vec![]),
                );
                typed_var_decls.push(ast::VariableDeclaration {
                    name: var_decl.name,
                    var_type,
                    init: Some(typed_init),
                })
            }
            None => {
                symbols::add_static_var(
                    var_decl.name.clone(),
                    var_decl.var_type.clone(),
                    true,
                    symbols::InitialValue::NoInitializer,
                );
                typed_var_decls.push(ast::VariableDeclaration {
                    name: var_decl.name,
                    var_type: var_decl.var_type,
                    init: None,
                });
            }
        }
    }
    typed_var_decls
}

fn typecheck_init(init: ast::UntypedInitializer) -> ast::TypedInitializer {
    match init {
        ast::UntypedInitializer::SingleInit(e) => {
            ast::TypedInitializer::SingleInit(typecheck_exp(e))
        }
        ast::UntypedInitializer::CompoundInit(..) => {
            panic!("cannot assign compound init to var now")
        }
    }
}

fn typecheck_fn_decl(
    f: ast::FunctionDeclaration<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::FunctionDeclaration<ast::TypedInitializer, ast::TypedExp> {
    validate_type(f.fun_type.clone());
    match f.fun_type.clone() {
        types::Type::FunType {
            param_types,
            ret_type: _,
        } => {
            for (idx, param) in f.params.iter().enumerate() {
                symbols::add_automatic_var(param.clone(), param_types[idx].clone());
            }
        }
        _ => panic!("Internal error, function has non-function type"),
    }
    let typed_body = match f.body {
        Some(block_items) => {
            let mut typed_block_items = vec![];
            for block_item in block_items {
                typed_block_items.push(typecheck_block_item(block_item));
            }
            Some(typed_block_items)
        }
        None => None,
    };
    ast::FunctionDeclaration {
        name: f.name,
        params: f.params,
        fun_type: f.fun_type,
        body: typed_body,
    }
}

fn typecheck_for_init(
    init: ast::ForInit<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::ForInit<ast::TypedInitializer, ast::TypedExp> {
    match init {
        ast::ForInit::InitExp(e) => {
            let typed_e = match e {
                Some(_e) => Some(typecheck_exp(_e)),
                None => None,
            };
            ast::ForInit::InitExp(typed_e)
        }
        ast::ForInit::InitDecl(decl) => ast::ForInit::InitDecl(typecheck_local_var_decl(decl)),
    }
}

fn typecheck_global_decl(
    decl: ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>,
) -> ast::Declaration<ast::TypedInitializer, ast::TypedExp> {
    match decl {
        ast::Declaration::FunDecl(fd) => ast::Declaration::FunDecl(typecheck_fn_decl(fd)),
        ast::Declaration::VarDecl(vd) => {
            ast::Declaration::VarDecl(typecheck_file_scope_var_decl(vd))
        }
    }
}

pub fn typecheck(prog: ast::UntypedProgram) -> ast::TypedProgram {
    let mut typed_decls = vec![];
    for decl in prog {
        typed_decls.push(typecheck_global_decl(decl));
    }
    typed_decls
}
