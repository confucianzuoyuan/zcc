package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
)

type TokenKind int

const (
	TK_IDENT   TokenKind = iota // Identifiers
	TK_PUNCT                    // Punctuators
	TK_KEYWORD                  // Keywords
	TK_STR                      // String literals
	TK_NUM                      // Numeric literals
	TK_EOF                      // End-of-file markers
)

type Token struct {
	Kind          TokenKind // Token kind
	Next          *Token    // Next token
	Value         int64     // If kind is TK_NUM, its value
	FloatValue    float64   // If kind is TK_NUM, its value
	Location      int       // Token location
	Length        int       // Token length
	Ty            *CType    // Used if TK_NUM or TK_STR
	StringLiteral string    // String literal contents including terminating '\0'

	LineNo int // Line number
}

var currentFilename string
var currentInput *[]uint8

func newToken(kind TokenKind, start int, end int) *Token {
	tok := &Token{
		Kind:     kind,
		Location: start,
		Length:   end - start,
	}

	return tok
}

/*
 * Reports an error message in the following format and exit.
 *
 * foo.c:10: x = y + 1;
 *               ^ <error message here>
 */
func verrorAt(buf *[]uint8, line_no int, loc int, msg string) {
	// Find a line containing `loc`
	line := loc
	for 0 < line && (*buf)[line-1] != '\n' {
		line -= 1
	}

	end := loc
	for (*buf)[end] != 0 && (*buf)[end] != '\n' {
		end += 1
	}

	// Print out the line.
	indent, _ := fmt.Fprintf(os.Stderr, "%s:%d: ", currentFilename, line_no)
	fmt.Fprintf(os.Stderr, "%s", string((*buf)[line:end+1]))

	// Show the error message
	pos := loc - line + indent

	fmt.Fprintf(os.Stderr, "%*s", pos, "") // print pos spaces.
	fmt.Fprintf(os.Stderr, "^ %s\n", msg)
	os.Exit(1)
}

func errorAt(loc int, msg string) {
	buf := currentInput
	line_no := 1
	for p := 0; p < loc; p += 1 {
		if (*buf)[p] == '\n' {
			line_no += 1
		}
	}

	verrorAt(buf, line_no, loc, msg)
	os.Exit(1)
}

func errorTok(tok *Token, msg string) {
	verrorAt(currentInput, tok.LineNo, tok.Location, msg)
	os.Exit(1)
}

func (t *Token) isEqual(s string) bool {
	return string((*currentInput)[t.Location:t.Location+t.Length]) == s
}

// Returns true if c is valid as the first character of an identifier.
func isIdentFirstChar(c uint8) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
}

// Returns true if c is valid as a non-first character of an identifier.
func isIdentInnerChar(c uint8) bool {
	return isIdentFirstChar(c) || ('0' <= c && c <= '9')
}

func fromHex(c uint8) uint8 {
	if '0' <= c && c <= '9' {
		return c - '0'
	}

	if 'a' <= 0 && c <= 'f' {
		return c - 'a' + 10
	}

	return c - 'A' + 10
}

func (tok *Token) isKeyword() bool {
	keywords := []string{
		"return", "if", "else", "for", "while", "int", "sizeof", "char",
		"struct", "union", "short", "long", "void", "typedef", "_Bool",
		"enum", "static", "goto", "break", "continue", "switch", "case",
		"default", "extern", "_Alignof", "_Alignas", "do", "signed",
		"unsigned", "const", "volatile", "auto", "register", "restrict",
		"__restrict", "__restrict__", "_Noreturn", "float", "double",
		"typeof", "asm", "_Thread_local", "__thread", "_Atomic",
		"__attribute__",
	}

	for _, k := range keywords {
		if tok.isEqual(k) {
			return true
		}
	}
	return false
}

func isHexDigit(c uint8) bool {
	if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') {
		return true
	}
	return false
}

func isBinDigit(c uint8) bool {
	return c == '0' || c == '1'
}

func isOctalDigit(c uint8) bool {
	return c >= '0' && c <= '7'
}

// 返回：(读取的值, new_pos)
func readEscapedChar(p int) (int8, int) {
	buf := currentInput
	if '0' <= (*buf)[p] && (*buf)[p] <= '7' {
		// Read an octal number.
		c := int((*buf)[p] - '0')
		p += 1
		if '0' <= (*buf)[p] && (*buf)[p] <= '7' {
			c = (c << 3) + int((*buf)[p]-'0')
			p += 1
			if '0' <= (*buf)[p] && (*buf)[p] <= '7' {
				c = (c << 3) + int((*buf)[p]-'0')
				p += 1
			}
		}

		return int8(c), p
	}

	if (*buf)[p] == 'x' {
		// Read a hexadecimal number
		p += 1
		if !isHexDigit((*buf)[p]) {
			errorAt(p, "invalid hex escape sequence")
		}

		c := int(0)
		for ; isHexDigit((*buf)[p]); p += 1 {
			c = (c << 4) + int(fromHex((*buf)[p]))
		}
		return int8(c), p
	}

	// Escape sequences are defined using themselves here. E.g.
	// '\n' is implemented using '\n'. This tautological definition
	// works because the compiler that compiles our compiler knows
	// what '\n' actually is. In other words, we "inherit" the ASCII
	// code of '\n' from the compiler that compiles our compiler,
	// so we don't have to teach the actual code here.
	//
	// This fact has huge implications not only for the correctness
	// of the compiler but also for the security of the generated code.
	// For more info, read "Reflections on Trusting Trust" by Ken Thompson.
	// https://github.com/rui314/chibicc/wiki/thompson1984.pdf
	switch (*buf)[p] {
	case 'a':
		return '\a', p + 1
	case 'b':
		return '\b', p + 1
	case 't':
		return '\t', p + 1
	case 'n':
		return '\n', p + 1
	case 'v':
		return '\v', p + 1
	case 'f':
		return '\f', p + 1
	case 'r':
		return '\r', p + 1
	case 'e':
		// [GNU] \e for the ASCII escape character is a GNU C extension.
		return 27, p + 1
	default:
		return int8((*buf)[p]), p + 1
	}
}

// Find a closing double-quote.
func stringLiteralEnd(p int) int {
	buf := currentInput
	start := p
	for ; (*buf)[p] != '"'; p += 1 {
		if (*buf)[p] == '\n' || (*buf)[p] == 0 {
			errorAt(start, "unclosed string literal")
		}
		if (*buf)[p] == '\\' {
			p += 1
		}
	}
	return p
}

func readStringLiteral(start int) *Token {
	buf := currentInput
	end := stringLiteralEnd(start + 1)
	str := make([]uint8, end-start)
	var len int64 = 0

	for p := start + 1; p < end; {
		if (*buf)[p] == '\\' {
			c, new_pos := readEscapedChar(p + 1)
			str[len] = uint8(c)
			len += 1
			p = new_pos
		} else {
			str[len] = (*buf)[p]
			len += 1
			p += 1
		}
	}

	tok := newToken(TK_STR, start, end+1)
	tok.Ty = arrayOf(TyChar, len+1)
	tok.StringLiteral = string(str)
	return tok
}

func readCharLiteral(start int) *Token {
	p := start + 1
	if (*currentInput)[p] == 0 {
		errorAt(start, "unclosed char literal")
	}

	var c int8
	if (*currentInput)[p] == '\\' {
		c, p = readEscapedChar(p + 1)
	} else {
		c = int8((*currentInput)[p])
		p += 1
	}

	end := p
	for (*currentInput)[end] != '\'' && end < len(*currentInput) {
		end += 1
	}
	if end == len(*currentInput) {
		errorAt(p, "unclosed char literal")
	}

	tok := newToken(TK_NUM, start, end+1)
	tok.Value = int64(c)
	tok.Ty = TyInt
	return tok
}

func readIntLiteral(start int) *Token {
	p := start

	// Read a binary, octal, decimal or hexadecimal number.
	base := 10
	if ((*currentInput)[p] == '0' && ((*currentInput)[p+1] == 'x' || (*currentInput)[p+1] == 'X')) && isHexDigit((*currentInput)[p+2]) {
		p += 2
		base = 16
	} else if ((*currentInput)[p] == '0' && ((*currentInput)[p+1] == 'b' || (*currentInput)[p+1] == 'B')) && isBinDigit((*currentInput)[p+2]) {
		p += 2
		base = 2
	} else if (*currentInput)[p] == '0' {
		base = 8
	}

	var val uint64
	if base == 10 {
		end := p
		for isDecimalDigit((*currentInput)[end]) {
			end += 1
		}
		val, _ = strconv.ParseUint(string((*currentInput)[p:end]), 10, 64)
		p = end
	} else if base == 2 {
		end := p
		for isBinDigit((*currentInput)[end]) {
			end += 1
		}
		val, _ = strconv.ParseUint(string((*currentInput)[p:end]), 2, 64)
		p = end
	} else if base == 16 {
		end := p
		for isHexDigit((*currentInput)[end]) {
			end += 1
		}
		val, _ = strconv.ParseUint(string((*currentInput)[p:end]), 16, 64)
		p = end
	} else if base == 8 {
		end := p
		for isOctalDigit((*currentInput)[end]) {
			end += 1
		}
		val, _ = strconv.ParseUint(string((*currentInput)[p:end]), 8, 64)
		p = end
	} else {
		errorAt(p, "invalid base")
	}

	// Read U, L or LL suffixes.
	l := false
	u := false

	suffix3 := string((*currentInput)[p : p+3])
	suffix2 := string((*currentInput)[p : p+2])
	suffix1 := string((*currentInput)[p : p+1])
	if suffix3 == "LLU" || suffix3 == "LLu" || suffix3 == "llU" || suffix3 == "llu" || suffix3 == "ULL" || suffix3 == "Ull" || suffix3 == "uLL" || suffix3 == "ull" {
		p += 3
		u = true
		l = true
	} else if suffix2 == "lu" || suffix2 == "Lu" || suffix2 == "lU" || suffix2 == "LU" || suffix2 == "ul" || suffix2 == "Ul" || suffix2 == "uL" || suffix2 == "UL" {
		p += 2
		l = true
		u = true
	} else if suffix2 == "LL" || suffix2 == "ll" {
		p += 2
		l = true
	} else if suffix1 == "L" || suffix1 == "l" {
		p += 1
		l = true
	} else if suffix1 == "U" || suffix1 == "u" {
		p += 1
		u = true
	}

	// Infer a type
	var ty *CType = nil
	if base == 10 {
		if l && u {
			ty = TyULong
		} else if l {
			ty = TyLong
		} else if u {
			if val>>32 != 0 {
				ty = TyULong
			} else {
				ty = TyUInt
			}
		} else {
			if val>>31 != 0 {
				ty = TyLong
			} else {
				ty = TyInt
			}
		}
	} else {
		if l && u {
			ty = TyULong
		} else if l {
			if val>>63 != 0 {
				ty = TyULong
			} else {
				ty = TyLong
			}
		} else if u {
			if val>>32 != 0 {
				ty = TyULong
			} else {
				ty = TyUInt
			}
		} else if val>>63 != 0 {
			ty = TyULong
		} else if val>>32 != 0 {
			ty = TyLong
		} else if val>>31 != 0 {
			ty = TyUInt
		} else {
			ty = TyInt
		}
	}

	tok := newToken(TK_NUM, start, p)
	tok.Value = int64(val)
	tok.Ty = ty
	return tok
}

func readNumber(start int) *Token {
	// Try to parse as an integer constant.
	tok := readIntLiteral(start)
	c := (*currentInput)[start+tok.Length]
	if c != '.' && c != 'e' && c != 'E' && c != 'f' && c != 'F' {
		return tok
	}

	// If it's not an integer, it must be a floating point constant.
	end := start
	for ((*currentInput)[end] <= '9' && (*currentInput)[end] >= '0') || ((*currentInput)[end] <= 'Z' && (*currentInput)[end] >= 'A') || ((*currentInput)[end] <= 'z' && (*currentInput)[end] >= 'a') || ((*currentInput)[end] == 'l' || (*currentInput)[end] == 'L') || (*currentInput)[end] == '.' || (*currentInput)[end] == '+' || (*currentInput)[end] == '-' {
		end += 1
	}

	var ty *CType
	var value float64
	if (*currentInput)[end-1] == 'f' || (*currentInput)[end-1] == 'F' {
		value, _ = strconv.ParseFloat(string((*currentInput)[start:end-1]), 64)
		ty = TyFloat
	} else if (*currentInput)[end-1] == 'l' || (*currentInput)[end-1] == 'L' {
		value, _ = strconv.ParseFloat(string((*currentInput)[start:end]), 64)
		ty = TyDouble
	} else {
		value, _ = strconv.ParseFloat(string((*currentInput)[start:end]), 64)
		ty = TyDouble
	}

	tok = newToken(TK_NUM, start, end)
	tok.FloatValue = value
	tok.Ty = ty
	return tok
}

func convertKeywords(tok *Token) {
	for t := tok; t.Kind != TK_EOF; t = t.Next {
		if t.isKeyword() {
			t.Kind = TK_KEYWORD
		}
	}
}

// Initialize line info for all tokens.
func addLineNumbers(tok *Token) {
	buf := currentInput
	p := 0
	n := 1

	if p == tok.Location {
		tok.LineNo = n
		tok = tok.Next
	}
	if (*buf)[p] == '\n' {
		n += 1
	}

	for ; (*buf)[p] != 0; p += 1 {
		if p == tok.Location {
			tok.LineNo = n
			tok = tok.Next
		}
		if (*buf)[p] == '\n' {
			n += 1
		}
	}
}

func isDecimalDigit(c uint8) bool {
	if c >= '0' && c <= '9' {
		return true
	}
	return false
}

func isAlphaNumber(c uint8) bool {
	return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

// Tokenize a given string and returns new tokens.
func tokenize(filename string, src *[]uint8) *Token {
	currentFilename = filename
	currentInput = src

	p := 0

	head := Token{}
	cur := &head

	for (*src)[p] != 0 {
		// Skip line comments.
		if (*src)[p] == '/' && (*src)[p+1] == '/' {
			p += 2
			for (*src)[p] != '\n' {
				p += 1
			}
			continue
		}

		// Character literal
		if (*src)[p] == '\'' {
			cur.Next = readCharLiteral(p)
			cur = cur.Next
			p += cur.Length
			continue
		}

		// Skip block comments
		if (*src)[p] == '/' && (*src)[p+1] == '*' {
			q := p + 2
			for (*src)[q] != 0 && (*src)[q+1] != 0 {
				if (*src)[q] == '*' && (*src)[q+1] == '/' {
					break
				}
				q += 1
			}
			if (*src)[q] != '*' {
				panic("unclosed block comment")
			}
			p = q + 2
			continue
		}

		// Skip whitespace characters.
		if (*src)[p] == ' ' || (*src)[p] == '\t' || (*src)[p] == '\v' || (*src)[p] == '\f' || (*src)[p] == '\n' || (*src)[p] == '\r' {
			p += 1
			continue
		}

		// Numeric literal
		if isDecimalDigit((*src)[p]) || ((*src)[p] == '.' && isDecimalDigit((*src)[p+1])) {
			cur.Next = readNumber(p)
			cur = cur.Next
			p += cur.Length
			continue
		}

		// String literal
		if (*src)[p] == '"' {
			cur.Next = readStringLiteral(p)
			cur = cur.Next
			p += cur.Length
			continue
		}

		// Identifier or keyword
		if isIdentFirstChar((*src)[p]) {
			start := p
			p += 1
			for isIdentInnerChar((*src)[p]) {
				p += 1
			}
			cur.Next = newToken(TK_IDENT, start, p)
			cur = cur.Next
			continue
		}

		// Punctuators
		punctLen := readPunct(p)
		if punctLen > 0 {
			cur.Next = newToken(TK_PUNCT, p, p+punctLen)
			cur = cur.Next
			p += cur.Length
			continue
		}

		errorAt(p, "invalid token")
	}

	cur.Next = newToken(TK_EOF, p, p)
	addLineNumbers(head.Next)
	convertKeywords(head.Next)
	return head.Next
}

// Read a punctuator token from p and returns its length.
func readPunct(p int) int {
	punctuators := []string{
		"<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=",
		"-=", "*=", "/=", "++", "--", "%=", "&=", "|=", "^=", "&&",
		"||", "<<", ">>", "##",

		// single char
		"<", ">", "=", "-", "!", "&", "|", "%", "(", ")", "[", "]", "{", "}", ";", ":",
		"#", ",", ".", "+", "-", "*", "/", "?", "~", "^",
	}

	for _, punct := range punctuators {
		punctLen := len(punct)
		if string((*currentInput)[p:p+punctLen]) == punct {
			return punctLen
		}
	}

	return 0
}

// Returns the contents of a given file.
func readFile(path string) *[]uint8 {
	var src []uint8
	var err error
	if path == "-" {
		// By convention, read from stdin if a given filename is "-"
		// 读取整个输入
		src, err = io.ReadAll(os.Stdin)
		if err != nil {
			fmt.Fprintln(os.Stderr, "读取输入时出错：", err)
			os.Exit(1)
		}

	} else {
		src, err = os.ReadFile(path)
		if err != nil {
			fmt.Fprintln(os.Stderr, "读取文件时出错：", err)
			os.Exit(1)
		}
	}

	// Make sure that the last line is properly terminated with '\n'.
	if src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	// add '\0' in the end of src
	src = append(src, 0)

	return &src
}

func tokenizeFile(path string) *Token {
	return tokenize(path, readFile(path))
}
