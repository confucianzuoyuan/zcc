pub static mut COUNT: i64 = 0;

pub fn new_unique_name() -> String {
    unsafe {
        let label = format!(".L..{}", COUNT);
        COUNT += 1;
        label
    }
}
