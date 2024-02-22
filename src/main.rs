fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args[0]);
    }

    let mut s = args[1].chars().into_iter().peekable();
    let mut t = String::new();

    println!("  .globl main");
    println!("main:");

    while s.peek().is_some() {
        if s.peek().unwrap().is_ascii_digit() {
            t.push(*s.peek().unwrap());
            s.next();
        } else {
            break;
        }
    }

    println!("  mov ${}, %rax", t);

    while s.peek().is_some() {
        if *s.peek().unwrap() == '+' {
            s.next();
            let mut t = String::new();
            while s.peek().is_some() {
                if s.peek().unwrap().is_ascii_digit() {
                    t.push(*s.peek().unwrap());
                    s.next();
                } else {
                    break;
                }
            }

            println!("  add ${}, %rax", t);
            continue;
        }

        if *s.peek().unwrap() == '-' {
            s.next();
            let mut t = String::new();
            while s.peek().is_some() {
                if s.peek().unwrap().is_ascii_digit() {
                    t.push(*s.peek().unwrap());
                    s.next();
                } else {
                    break;
                }
            }

            println!("  sub ${}, %rax", t);
            continue;
        }

        panic!("unexpected character: {}", s.peek().unwrap());
    }

    println!("  ret");
}
