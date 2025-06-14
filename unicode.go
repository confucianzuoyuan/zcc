package main

// Encode a given character in UTF-8.
func encodeUTF8(buf *[]uint8, offset int, c uint32) int {
	if c <= 0x7F {
		(*buf)[offset] = uint8(c)
		return 1
	}

	if c <= 0x7FF {
		(*buf)[offset] = uint8(0b11000000 | (c >> 6))
		(*buf)[offset+1] = uint8(0b10000000 | (c & 0b00111111))
		return 2
	}

	if c <= 0xFFFF {
		(*buf)[offset+0] = uint8(0b11100000 | (c >> 12))
		(*buf)[offset+1] = uint8(0b10000000 | ((c >> 6) & 0b00111111))
		(*buf)[offset+2] = uint8(0b10000000 | (c & 0b00111111))
		return 3
	}

	(*buf)[offset+0] = uint8(0b11110000 | (c >> 18))
	(*buf)[offset+1] = uint8(0b10000000 | ((c >> 12) & 0b00111111))
	(*buf)[offset+2] = uint8(0b10000000 | ((c >> 6) & 0b00111111))
	(*buf)[offset+3] = uint8(0b10000000 | (c & 0b00111111))

	return 4
}

// Read a UTF-8-encoded Unicode code point from a source file.
// We assume that source files are always in UTF-8.
//
// UTF-8 is a variable-width encoding in which one code point is
// encoded in one to four bytes. One byte UTF-8 code points are
// identical to ASCII. Non-ASCII characters are encoded using more
// than one byte.
func decodeUTF8(buf *[]uint8, p int) (uint32, int) {
	newPos := 0
	if (*buf)[p] < 128 {
		newPos = p + 1
		return uint32((*buf)[p]), newPos
	}

	start := p
	len := 0
	c := uint32(0)

	if (*buf)[p] >= 0b11110000 {
		len = 4
		c = uint32((*buf)[p] & 0b111)
	} else if (*buf)[p] >= 0b11100000 {
		len = 3
		c = uint32((*buf)[p] & 0b1111)
	} else if (*buf)[p] >= 0b11000000 {
		len = 2
		c = uint32((*buf)[p] & 0b11111)
	} else {
		errorAt(start, "invalid UTF-8 sequence")
	}

	for i := 1; i < len; i += 1 {
		if (*buf)[p+i]>>6 != 0b10 {
			errorAt(start, "invalid UTF-8 sequence")
		}
		c = (c << 6) | uint32(((*buf)[p+i] & 0b111111))
	}

	newPos = p + len
	return c, newPos
}
