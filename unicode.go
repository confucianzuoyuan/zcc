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
