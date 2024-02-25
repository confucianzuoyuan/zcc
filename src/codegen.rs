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
        // 数值类型直接写入rax寄存器
        ast::Expr::Int { value } => println!("  mov ${}, %rax", value),
        // gen_expr最终的求值结果在rax寄存器中
        ast::Expr::Unary { oper: _, expr } => {
            gen_expr(*expr);
            println!(" neg %rax");
        }
        // 将right的求值结果先写入rax寄存器
        // 再将rax中的值压栈
        // 然后将left的求值结果写入rax
        // 然后将栈中的值(也就是right的求值结果)弹入rdi寄存器
        // 然后对rdi(right的值)和rax(left的值)做二元计算
        ast::Expr::Binary { left, oper, right } => {
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
    }
}

pub fn gen_stmt(node: ast::StmtWithPos) {
    match node.node {
        ast::Stmt::Expr(e) => gen_expr(e),
    }
}
