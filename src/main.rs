use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args[0]);
    }
    println!("  .globl main");
    println!("main:");
    println!("  mov ${}, %rax", args[1].parse::<i32>().unwrap());
    println!("  ret");
}
