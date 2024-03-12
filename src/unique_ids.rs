pub static mut COUNT: i64 = 0;

pub fn new_unique_name() -> String {
    unsafe {
        let label = format!(".L..{}", COUNT);
        COUNT += 1;
        label
    }
}

pub fn make_temporary() -> String {
    unsafe {
        let n = COUNT;
        COUNT = n + 1;
        format!("tmp.{}", n)
    }
}

pub fn make_label(prefix: String) -> String {
    unsafe {
        let n = COUNT;
        COUNT = n + 1;
        format!("{}.{}", prefix, n)
    }
}

pub fn make_named_temporary(prefix: String) -> String {
    unsafe {
        let n = COUNT;
        COUNT = n + 1;
        format!("{}.{}", prefix, n)
    }
}
