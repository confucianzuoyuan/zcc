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