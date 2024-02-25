use crate::ast;

pub static mut DEPTH: i64 = 0;

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

pub fn gen_expr(node: ast::ExprWithPos) {
    match node.node {
        ast::Expr::Int { value } => println!("  mov ${}, %rax", value),
        ast::Expr::Oper { left, oper, right } => {
            gen_expr(*right);
            push();
            gen_expr(*left);
            pop("%rdi".to_string());

            match oper.node {
                ast::Operator::Plus => println!("  add %rdi, %rax"),
                ast::Operator::Minus => println!("  sub %rdi, %rax"),
                ast::Operator::Times => println!("  imul %rdi, %rax"),
                ast::Operator::Divide => {
                    println!("  cqo");
                    println!("  idiv %rdi");
                }
            }
        }
    }
}
