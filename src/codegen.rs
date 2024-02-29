use std::borrow::BorrowMut;

use crate::{ast, parser};

pub static mut DEPTH: i64 = 0;
pub static mut COUNT: i64 = 0;
pub static ARG_REG: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

fn count() -> i64 {
    unsafe {
        COUNT += 1;
        COUNT
    }
}

fn depth_inc() {
    unsafe {
        DEPTH += 1;
    }
}

fn depth_dec() {
    unsafe {
        DEPTH -= 1;
    }
}

fn push() {
    println!("  push %rax");
    depth_inc();
}

fn pop(arg: String) {
    println!("  pop {}", arg);
    depth_dec();
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn gen_addr(node: ast::TypedExpr) {
    match node.expr {
        ast::InnerTypedExpr::Var(v) => {
            let offset = parser::get_var_offset(v.name);
            println!("  lea {}(%rbp), %rax", offset);
        }
        ast::InnerTypedExpr::Deref(e) => {
            gen_expr(*e);
        }
        _ => panic!("not an lvalue: {:?}", node),
    }
}

pub fn gen_expr(node: ast::TypedExpr) {
    match node.expr {
        // 数值类型直接写入rax寄存器
        ast::InnerTypedExpr::Int { value } => println!("  mov ${}, %rax", value),
        // gen_expr最终的求值结果在rax寄存器中
        ast::InnerTypedExpr::Unary { oper: _, expr } => {
            gen_expr(*expr);
            println!(" neg %rax");
        }
        // 将right的求值结果先写入rax寄存器
        // 再将rax中的值压栈
        // 然后将left的求值结果写入rax
        // 然后将栈中的值(也就是right的求值结果)弹入rdi寄存器
        // 然后对rdi(right的值)和rax(left的值)做二元计算
        ast::InnerTypedExpr::Binary { left, oper, right } => {
            gen_expr(*right);
            push();
            gen_expr(*left);
            pop("%rdi".to_string());

            match oper {
                ast::Operator::Plus => println!("  add %rdi, %rax"),
                ast::Operator::Minus => println!("  sub %rdi, %rax"),
                ast::Operator::Times => println!("  imul %rdi, %rax"),
                ast::Operator::Divide => {
                    println!("  cqo");
                    println!("  idiv %rdi");
                }
                ast::Operator::Equal => {
                    println!("  cmp %rdi, %rax");
                    println!("  sete %al");
                    println!("  movzb %al, %rax");
                }
                ast::Operator::NotEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setne %al");
                    println!("  movzb %al, %rax");
                }
                ast::Operator::LesserThan => {
                    println!("  cmp %rdi, %rax");
                    println!("  setl %al");
                    println!("  movzb %al, %rax");
                }
                ast::Operator::LesserOrEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setle %al");
                    println!("  movzb %al, %rax");
                }
                ast::Operator::GreaterThan => {
                    println!("  cmp %rdi, %rax");
                    println!("  setg %al");
                    println!("  movzb %al, %rax");
                }
                ast::Operator::GreaterOrEqual => {
                    println!("  cmp %rdi, %rax");
                    println!("  setge %al");
                    println!("  movzb %al, %rax");
                }
            }
        }
        ast::InnerTypedExpr::Assign { lvalue, rvalue } => {
            gen_addr(*lvalue);
            push();
            gen_expr(*rvalue);
            pop("%rdi".to_string());
            println!("  mov %rax, (%rdi)");
        }
        ast::InnerTypedExpr::Var(_) => {
            gen_addr(node);
            println!("  mov (%rax), %rax");
        }
        ast::InnerTypedExpr::Deref(e) => {
            gen_expr(*e);
            println!("  mov (%rax), %rax");
        }
        ast::InnerTypedExpr::Addr(e) => {
            gen_addr(*e);
        }
        ast::InnerTypedExpr::FunCall { funcname, args } => {
            let mut nargs = 0;
            for arg in args {
                gen_expr(arg);
                push();
                nargs += 1;
            }
            let mut i: i32 = nargs - 1;
            while i >= 0 {
                pop(ARG_REG[i as usize].to_string());
                i -= 1;
            }
            println!("  mov $0, %rax");
            println!("  call {}", funcname);
        }
    }
}

fn assign_lvar_offsets(prog: &mut ast::Function<ast::TypedExpr>) {
    let mut offset: i64 = 0;
    let vars = parser::get_vars();
    for v in vars {
        offset += 8;
        parser::update_var_offset(v, -offset);
    }
    prog.stack_size = align_to(offset, 16);
}

pub fn gen_stmt(node: ast::StmtWithPos<ast::TypedExpr>) {
    match node.node {
        ast::Stmt::Expr(e) => gen_expr(e),
        ast::Stmt::Return(e) => {
            gen_expr(e);
            println!("  jmp .L.return");
        }
        ast::Stmt::Block(block) => {
            for stmt in block {
                gen_stmt(stmt);
            }
        }
        ast::Stmt::Null => (),
        ast::Stmt::If { cond, then, els } => {
            let c = count();
            gen_expr(*cond);
            println!("  cmp $0, %rax");
            println!("  je .L.else.{}", c);
            gen_stmt(*then);
            println!("  jmp .L.end.{}", c);
            println!(".L.else.{}:", c);
            if let Some(_else) = *els {
                gen_stmt(_else);
            }
            println!(".L.end.{}:", c);
        }
        ast::Stmt::For {
            cond,
            then,
            init,
            inc,
        } => {
            let c = count();
            gen_stmt(*init);
            println!(".L.begin.{}:", c);
            if let Some(cond) = *cond {
                gen_expr(cond);
                println!("  cmp $0, %rax");
                println!("  je .L.end.{}", c);
            }
            gen_stmt(*then);
            if let Some(inc) = *inc {
                gen_expr(inc);
            }
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
        ast::Stmt::While { cond, then } => {
            let c = count();
            println!(".L.begin.{}:", c);
            gen_expr(*cond);
            println!("  cmp $0, %rax");
            println!("  je .L.end.{}", c);
            gen_stmt(*then);
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
    }
}

pub fn codegen(mut prog: ast::Function<ast::TypedExpr>) {
    assign_lvar_offsets(&mut prog);

    println!("  .globl main");
    println!("main:");

    // Prologue
    println!("  push %rbp");
    println!("  mov %rsp, %rbp");
    println!("  sub ${}, %rsp", prog.stack_size);

    gen_stmt(*prog.body);
    unsafe {
        assert!(DEPTH == 0);
    }

    println!(".L.return:");

    // Epilogue
    println!("  mov %rbp, %rsp");
    println!("  pop %rbp");
    println!("  ret");
}
