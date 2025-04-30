/// Encode a given character in UTF-8.
pub fn encode_utf8(buf: &mut [u8], c: u32) -> i32 {
    if c <= 0x7F {
        buf[0] = c as u8;
        return 1;
    }

    if c <= 0x7FF {
        buf[0] = 0b11000000 | (c >> 6) as u8;
        buf[1] = 0b10000000 | (c & 0b00111111) as u8;
        return 2;
    }

    if c <= 0xFFFF {
        buf[0] = 0b11100000 | (c >> 12) as u8;
        buf[1] = 0b10000000 | ((c >> 6) & 0b00111111) as u8;
        buf[2] = 0b10000000 | (c & 0b00111111) as u8;
        return 3;
    }
    buf[0] = 0b11110000 | (c >> 18) as u8;
    buf[1] = 0b10000000 | ((c >> 12) & 0b00111111) as u8;
    buf[2] = 0b10000000 | ((c >> 6) & 0b00111111) as u8;
    buf[3] = 0b10000000 | (c & 0b00111111) as u8;
    return 4;
}

/// Read a UTF-8-encoded Unicode code point from a source file.
/// We assume that source files are always in UTF-8.
///
/// UTF-8 is a variable-width encoding in which one code point is
/// encoded in one to four bytes. One byte UTF-8 code points are
/// identical to ASCII. Non-ASCII characters are encoded using more
/// than one byte.
pub fn decode_utf8(buf: &[u8], start: usize) -> (u32, usize) {
    if buf[start] < 128 {
        return (buf[start] as u32, start + 1);
    }

    let mut len = 0;
    let mut c: u32 = 0;

    if buf[start] >= 0b11110000 {
        len = 4;
        c = (buf[start] & 0b111) as u32;
    } else if buf[start] >= 0b11100000 {
        len = 3;
        c = (buf[start] & 0b1111) as u32;
    } else if buf[start] >= 0b11000000 {
        len = 2;
        c = (buf[start] & 0b11111) as u32;
    } else {
        return (0, start);
    }

    for i in 1..len {
        c = (c << 6) | (buf[start + i] & 0b00111111) as u32;
    }

    (c, start + len)
}

fn in_range(range: &[u32], c: u32) -> bool {
    let mut i = 0;
    loop {
        if range[i] != u32::MAX {
            if range[i] <= c && c <= range[i + 1] {
                return true;
            } else {
                i += 2;
            }
        } else {
            break;
        }
    }
    false
}

/// [https://www.sigbus.info/n1570#D] C11 allows not only ASCII but
/// some multibyte characters in certan Unicode ranges to be used in an
/// identifier.
///
/// This function returns true if a given character is acceptable as
/// the first character of an identifier.
///
/// For example, ¾ (U+00BE) is a valid identifier because characters in
/// 0x00BE-0x00C0 are allowed, while neither ⟘ (U+27D8) nor '　'
/// (U+3000, full-width space) are allowed because they are out of range.
pub fn is_identifier_start(c: u32) -> bool {
    let ranges = [
        0x0041,
        0x005A, // A-Z
        0x0061,
        0x007A, // a-z
        0x00C0,
        0x00FF, // Latin-1 Supplement
        0x0100,
        0x017F, // Latin Extended A
        0x0180,
        0x024F, // Latin Extended B
        0x0370,
        0x03FF, // Greek and Coptic
        0x0401,
        0x052F, // Cyrillic
        0x0531,
        0x058F, // Armenian
        0x0591,
        0x05FF, // Hebrew
        0x0601,
        0x06FF, // Arabic
        0x0701,
        0x074F, // Syriac
        0x0780,
        0x07BF, // Thaana
        0x0901,
        0x097F, // Devanagari
        0x0981,
        0x09FF, // Bengali
        0x0A01,
        0x0A7F, // Gurmukhi
        u32::MAX,
    ];
    in_range(&ranges[..], c)
}

/// Returns true if a given character is acceptable as a non-first
/// character of an identifier.
pub fn is_identifier(c: u32) -> bool {
    let range = [
        '0' as u32,
        '9' as u32, // 0-9
        '$' as u32,
        '$' as u32,
        0x0300,
        0x036F,
        0x1DC0,
        0x1DFF,
        0x20D0,
        0x20FF,
        0xFE20,
        0xFE2F,
        u32::MAX,
    ];
    is_identifier_start(c) || in_range(&range[..], c)
}

// Returns the number of columns needed to display a given
// character in a fixed-width font.
//
// Based on https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
fn char_width(c: u32) -> i32 {
    if c == 0 {
        return -1;
    }
    if c < 0x20 {
        return 0;
    }
    if c < 0x7F {
        return 1;
    }
    if c == 0x20E3 {
        return 0;
    }
    if c >= 0x1100
        && (c <= 0x115F
            || c == 0x2329
            || c == 0x232A
            || (c >= 0x2E80 && c <= 0xA4CF && c != 0x303F)
            || (c >= 0xAC00 && c <= 0xD7A3)
            || (c >= 0xF900 && c <= 0xFAFF)
            || (c >= 0xFE10 && c <= 0xFE6F)
            || (c >= 0xFF00 && c <= 0xFFEF)
            || (c >= 0x1F900 && c <= 0x1FAFF))
    {
        return 2;
    }
    return -1;
}

// Returns the number of columns needed to display a given
// string in a fixed-width font.
pub fn display_width(s: &str) -> i32 {
    let mut width = 0;
    for c in s.chars() {
        let w = char_width(c as u32);
        if w < 0 {
            return -1;
        }
        width += w;
    }
    width
}
