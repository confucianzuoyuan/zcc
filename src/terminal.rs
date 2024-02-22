use std::io::stderr;
use std::os::raw::c_int;
use std::os::unix::io::AsRawFd;

// #include <stdio.h>
//
// int main() {
//     // ANSI 转义序列
//     const char* BLUE_BOLD = "\x1b[1;34m"; // 加粗和蓝色
//     const char* RESET = "\x1b[0m"; // 重置文本属性
//
//     printf("%sThis text is bold and blue%s\n", BLUE_BOLD, RESET);
//
//     return 0;
// }
const BOLD: &str = "\x1b[1m";
const BLUE: &str = "\x1b[34m";
const END_BOLD: &str = "\x1b[22m";
const RED: &str = "\x1b[31m";
const RESET_COLOR: &str = "\x1b[39;49m";

pub struct Terminal {
    is_a_tty: bool,
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            is_a_tty: stderr_is_a_tty(),
        }
    }

    pub fn bold(&self) -> &str {
        if self.is_a_tty {
            BOLD
        } else {
            ""
        }
    }

    pub fn blue(&self) -> &str {
        if self.is_a_tty {
            BLUE
        } else {
            ""
        }
    }

    pub fn end_bold(&self) -> &str {
        if self.is_a_tty {
            END_BOLD
        } else {
            ""
        }
    }

    pub fn red(&self) -> &str {
        if self.is_a_tty {
            RED
        } else {
            ""
        }
    }

    pub fn reset_color(&self) -> &str {
        if self.is_a_tty {
            RESET_COLOR
        } else {
            ""
        }
    }
}

fn stderr_is_a_tty() -> bool {
    unsafe { isatty(stderr().as_raw_fd()) != 0 }
}

extern "C" {
    fn isatty(fd: c_int) -> c_int;
}
