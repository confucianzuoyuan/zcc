use std::{collections::HashMap, sync::Mutex};

use crate::{ast, initializers, symbols, typecheck, types, unique_ids};
use lazy_static::lazy_static;

const ARG_REG8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REG64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
static mut COUNT: i64 = 0;
static mut DEPTH: i64 = 0;
static mut CURRENT_FN: Option<String> = None;

fn get_current_fn() -> String {
    unsafe { CURRENT_FN.clone().unwrap() }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    fun_name: String,
    vars_offsets: Vec<(String, i64)>,
    stack_size: i64,
}

lazy_static! {
    static ref FUN_INFOS: Mutex<HashMap<String, FunctionInfo>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

fn get_var_offset(fun_name: String, var_name: String) -> i64 {
    let map = FUN_INFOS.lock().unwrap();
    let vars_offsets = &map.get(&fun_name).unwrap().vars_offsets;
    for vo in vars_offsets {
        if vo.0 == var_name {
            return vo.1;
        }
    }
    0
}

fn set_var_offset(fun_name: String, var_name: String, offset: i64) {
    let mut map = FUN_INFOS.lock().unwrap();
    match map.get_mut(&fun_name) {
        Some(fun_info) => {
            fun_info.vars_offsets.push((var_name, offset));
        }
        None => {
            map.insert(
                fun_name.clone(),
                FunctionInfo {
                    fun_name,
                    vars_offsets: vec![(var_name, offset)],
                    stack_size: 0,
                },
            );
        }
    }
}

fn get_fun_stack_size(fun_name: String) -> i64 {
    let map = FUN_INFOS.lock().unwrap();
    map.get(&fun_name).unwrap().stack_size
}

fn set_fun_stack_size(fun_name: String, stack_size: i64) {
    let mut map = FUN_INFOS.lock().unwrap();
    match map.get_mut(&fun_name) {
        Some(fun_info) => {
            fun_info.stack_size = stack_size;
        }
        None => {
            map.insert(
                fun_name.clone(),
                FunctionInfo {
                    fun_name,
                    vars_offsets: vec![],
                    stack_size: 0,
                },
            );
        }
    }
}

fn count() -> i64 {
    unsafe {
        COUNT += 1;
        COUNT
    }
}

fn push() {
    println!("  push %rax");
    unsafe {
        DEPTH += 1;
    }
}

fn pop(arg: String) {
    println!("  pop {}", arg);
    unsafe {
        DEPTH -= 1;
    }
}

/// Round up `n` to the nearest multiple of `align`. For instance,
/// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn get_string_helper(entry: symbols::Entry) -> Vec<i8> {
    match entry.attrs {
        symbols::IdentifierAttrs::StaticAttr { init, .. } => match init {
            symbols::InitialValue::Initial(inits) => {
                let mut bytes = vec![];
                for init in inits {
                    match init {
                        initializers::StaticInit::CharInit(c) => bytes.push(c),
                        _ => panic!(),
                    }
                }
                bytes
            }
            _ => panic!(),
        },
        _ => panic!(),
    }
}

fn gen_addr(node: ast::TypedExp) {
    match node.e {
        ast::TypedInnerExp::Var(v) => {
            if symbols::is_global(v.clone()) {
                let entry = symbols::get(v.clone());
                match entry.t.clone() {
                    types::Type::Array { elem_type, .. } => match *elem_type {
                        types::Type::Char => {
                            let bytes = get_string_helper(entry);
                            println!("  .data");
                            println!("  .globl {}", v);
                            println!("{}:", v);
                            for b in bytes {
                                println!("  .byte {}", b);
                            }
                            println!("  .text");
                            println!("  lea {}(%rip), %rax", v);
                        }
                        _ => println!("  lea {}(%rip), %rax", v),
                    },
                    _ => println!("  lea {}(%rip), %rax", v),
                }
            } else {
                println!("  lea {}(%rbp), %rax", get_var_offset(get_current_fn(), v));
            }
        }
        ast::TypedInnerExp::Dereference(e) => gen_expr(*e),
        _ => panic!("not an lvalue"),
    }
}

/// Load a value from where %rax is pointing to.
fn load(ty: types::Type) {
    match ty {
        // If it is an array, do not attempt to load a value to the
        // register because in general we can't load an entire array to a
        // register. As a result, the result of an evaluation of an array
        // becomes not the array itself but the address of the array.
        // This is where "array is automatically converted to a pointer to
        // the first element of the array in C" occurs.
        types::Type::Array { .. } => (),
        types::Type::Char => println!("  movsbq (%rax), %rax"),
        _ => println!("  mov (%rax), %rax"),
    }
}

/// Store %rax to an address that the stack top is pointing to.
fn store(ty: types::Type) {
    pop("%rdi".to_string());
    match ty {
        types::Type::Char => println!("  mov %al, (%rdi)"),
        _ => println!("  mov %rax, (%rdi)"),
    }
}

fn gen_expr(typed_e: ast::TypedExp) {
    match typed_e.e {
        ast::TypedInnerExp::Constant(c) => println!("  mov ${}, %rax", c),
        ast::TypedInnerExp::Var(..) => {
            gen_addr(typed_e.clone());
            load(typed_e.t);
        }
        ast::TypedInnerExp::AddrOf(e) => gen_addr(*e),
        ast::TypedInnerExp::Assignment(lhs, rhs) => {
            gen_addr(*lhs);
            push();
            gen_expr(*rhs);
            store(typed_e.t);
        }
        ast::TypedInnerExp::Unary(_, typed_e) => {
            gen_expr(*typed_e);
            println!("  neg %rax");
        }
        ast::TypedInnerExp::Binary(op, left, right) => {
            gen_expr(*right);
            push();
            gen_expr(*left);
            pop("%rdi".to_string());

            match op {
                ast::BinaryOperator::Add => println!("  add %rdi, %rax"),
                ast::BinaryOperator::Subtract => println!("  sub %rdi, %rax"),
                ast::BinaryOperator::Multiply => println!("  imul %rdi, %rax"),
                ast::BinaryOperator::Divide => {
                    println!("  cqo");
                    println!("  idiv %rdi");
                }
                ast::BinaryOperator::Equal => {
                    println!("  cmp %rdi, %rax");
                    println!("  sete %al");
                    println!("  movzb %al, %rax");
                }
                ast::BinaryOperator::NotEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setne %al");
                    println!("  movzb %al, %rax");
                }
                ast::BinaryOperator::LessThan => {
                    println!("  cmp %rdi, %rax");
                    println!("  setl %al");
                    println!("  movzb %al, %rax");
                }
                ast::BinaryOperator::LessOrEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setle %al");
                    println!("  movzb %al, %rax");
                }
                ast::BinaryOperator::GreaterThan => {
                    println!("  cmp %rdi, %rax");
                    println!("  setg %al");
                    println!("  movzb %al, %rax");
                }
                ast::BinaryOperator::GreaterOrEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setge %al");
                    println!("  movzb %al, %rax");
                }
            }
        }
        ast::TypedInnerExp::Dereference(e) => {
            gen_expr(*e);
            load(typed_e.t);
        }
        ast::TypedInnerExp::FunCall { f, args } => {
            let mut nargs = 0;
            for arg in args {
                gen_expr(arg);
                push();
                nargs += 1;
            }

            let mut i = nargs - 1;
            while i >= 0 {
                pop(ARG_REG64[i as usize].to_string());
                i -= 1;
            }

            println!("  mov $0, %rax");
            println!("  call {}", f);
        }
        ast::TypedInnerExp::SizeOf(e) => {
            println!("  mov ${}, %rax", types::get_type_size(e.t));
        }
        ast::TypedInnerExp::SizeOfT(..) => panic!("not support sizeof Type now"),
    }
}

fn gen_stmt(typed_stmt: ast::Statement<ast::TypedInitializer, ast::TypedExp>) {
    match typed_stmt {
        ast::Statement::If {
            condition,
            then_clause,
            else_clause,
        } => {
            let c = count();
            gen_expr(condition);
            println!("  cmp $0, %rax");
            println!("  je .L.else.{}", c);
            gen_stmt(*then_clause);
            println!("  jmp .L.end.{}", c);
            println!(".L.else.{}:", c);
            if let Some(_else) = *else_clause {
                gen_stmt(_else);
            }
            println!(".L.end.{}:", c);
        }
        ast::Statement::Expression(e) => gen_expr(e),
        ast::Statement::Return(e) => {
            if let Some(_e) = e {
                gen_expr(_e);
            }
            println!("  jmp .L.return.{}", get_current_fn());
        }
        ast::Statement::Compound(block_items) => gen_block(block_items),
        ast::Statement::For {
            init,
            condition,
            post,
            body,
            id: _,
        } => {
            let c = count();
            match init {
                ast::ForInit::InitDecl(vd) => {
                    for var_init in vd {
                        if let Some(init) = var_init.init {
                            match init {
                                ast::TypedInitializer::SingleInit(e) => {
                                    println!(
                                        "  lea {}(%rbp), %rax",
                                        get_var_offset(get_current_fn(), var_init.name)
                                    );
                                    push();
                                    gen_expr(e);
                                    pop("%rdi".to_string());
                                    println!("  mov %rax, (%rdi)");
                                }
                                ast::TypedInitializer::CompoundInit(..) => panic!(),
                            }
                        }
                    }
                }
                ast::ForInit::InitExp(e) => {
                    if let Some(_e) = e {
                        gen_expr(_e);
                    }
                }
            }
            println!(".L.begin.{}:", c);
            if let Some(cond) = condition {
                gen_expr(cond);
                println!("  cmp $0, %rax");
                println!("  je .L.end.{}", c);
            }
            gen_stmt(*body);
            if let Some(_post) = post {
                gen_expr(_post);
            }
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
        ast::Statement::While {
            condition,
            body,
            id: _,
        } => {
            let c = count();
            println!(".L.begin.{}:", c);
            gen_expr(condition);
            println!("  cmp $0, %rax");
            println!("  je .L.end.{}", c);
            gen_stmt(*body);
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
        ast::Statement::Null => (),
    }
}

fn assign_lvar_offsets(fd: ast::FunctionDeclaration<ast::TypedInitializer, ast::TypedExp>) {
    let mut offset = 0;
    // 为参数开辟栈空间
    for param in fd.params {
        offset += 8;
        set_var_offset(get_current_fn(), param, -offset);
    }
    // 为局部变量开辟栈空间
    if let Some(block_items) = fd.body {
        for block_item in block_items.iter().rev() {
            match block_item {
                ast::BlockItem::D(d) => match d {
                    ast::Declaration::VarDecl(vd) => {
                        for var_init in vd.iter().rev() {
                            offset +=
                                types::get_type_size(symbols::get(var_init.name.clone()).t) as i64;
                            set_var_offset(get_current_fn(), var_init.name.clone(), -offset);
                        }
                    }
                    ast::Declaration::FunDecl(..) => {
                        panic!("not support fun decl in block")
                    }
                },
                ast::BlockItem::S(..) => (),
            }
        }
    }

    set_fun_stack_size(fd.name, align_to(offset, 16));
}

fn gen_block(block: ast::Block<ast::TypedInitializer, ast::TypedExp>) {
    for block_item in block {
        match block_item {
            ast::BlockItem::D(d) => match d {
                ast::Declaration::VarDecl(vd) => {
                    for var_init in vd.iter().rev() {
                        if let Some(init) = var_init.init.clone() {
                            match init {
                                ast::TypedInitializer::SingleInit(e) => {
                                    println!(
                                        "  lea {}(%rbp), %rax",
                                        get_var_offset(get_current_fn(), var_init.name.clone())
                                    );
                                    push();
                                    gen_expr(e);
                                    pop("%rdi".to_string());
                                    println!("  mov %rax, (%rdi)");
                                }
                                ast::TypedInitializer::CompoundInit(..) => panic!(),
                            }
                        }
                    }
                }
                ast::Declaration::FunDecl(..) => panic!("not support fun decl in block"),
            },
            ast::BlockItem::S(s) => gen_stmt(s),
        }
    }
}

pub fn gen(prog: ast::TypedProgram) {
    for f in prog {
        match f {
            ast::Declaration::FunDecl(fd) => {
                println!("  .text");
                unsafe {
                    CURRENT_FN = Some(fd.name.clone());
                }
                assign_lvar_offsets(fd.clone());
                println!("  .globl {}", fd.name);
                println!("{}:", fd.name);

                // Prologue
                println!("  push %rbp");
                println!("  mov %rsp, %rbp");
                println!("  sub ${}, %rsp", get_fun_stack_size(fd.name.clone()));

                // Save passed-by-register arguments to the stack
                let mut i = 0;
                for param in fd.params {
                    let t = symbols::get(param.clone()).t;
                    match t {
                        types::Type::Char => println!(
                            "  mov {}, {}(%rbp)",
                            ARG_REG8[i as usize].to_string(),
                            get_var_offset(fd.name.clone(), param)
                        ),
                        _ => println!(
                            "  mov {}, {}(%rbp)",
                            ARG_REG64[i as usize].to_string(),
                            get_var_offset(fd.name.clone(), param)
                        ),
                    }
                    i += 1;
                }

                // Emit code
                assert!(unsafe { DEPTH == 0 });
                if let Some(body) = fd.body {
                    gen_block(body);
                }

                // Epilogue
                println!(".L.return.{}:", fd.name);
                println!("  mov %rbp, %rsp");
                println!("  pop %rbp");
                println!("  ret");
            }
            ast::Declaration::VarDecl(vd) => {
                println!("  .data");
                for v in vd {
                    println!("  .globl {}", v.name);
                    println!("{}:", v.name);
                    println!("  .zero {}", types::get_type_size(symbols::get(v.name).t));
                }
            }
        }
    }
}
