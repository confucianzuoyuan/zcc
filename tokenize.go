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
	TK_PP_NUM                   // Preprocessing numbers
	TK_EOF                      // End-of-file markers
)

type File struct {
	Name     string
	FileNo   int
	Contents *[]uint8
}

type Token struct {
	Kind          TokenKind // Token kind
	Next          *Token    // Next token
	Value         int64     // If kind is TK_NUM, its value
	FloatValue    float64   // If kind is TK_NUM, its value
	Location      int       // Token location
	Length        int       // Token length
	Ty            *CType    // Used if TK_NUM or TK_STR
	StringLiteral string    // String literal contents including terminating '\0'

	File              *File    //Source location
	LineNo            int      // Line number
	AtBeginningOfLine bool     // True if this token is at beginning of line
	HasSpace          bool     // True if this token follows a space character
	Hideset           *Hideset // For macro expansion
	Origin            *Token   // If this is expanded from a macro, the original token
}

// Input file
var currentFile *File

// A list of all input files.
var inputFiles []*File

var fileNo int = 0

// True if the current position is at the beginning of a line
var atBeginningOfLine bool

// True if the current position follows a space character
var hasSpace bool

func newToken(kind TokenKind, start int, end int) *Token {
	tok := &Token{
		Kind:              kind,
		Location:          start,
		Length:            end - start,
		File:              currentFile,
		AtBeginningOfLine: atBeginningOfLine,
		HasSpace:          hasSpace,
	}

	atBeginningOfLine = false
	hasSpace = false
	return tok
}

func (t *Token) copy() *Token {
	newToken := &Token{}
	*newToken = *t
	newToken.Next = nil
	return newToken
}

// Append tok2 to the end of tok1.
func (t *Token) append(other *Token) *Token {
	if t.Kind == TK_EOF {
		return other
	}

	head := Token{}
	cur := &head

	for ; t.Kind != TK_EOF; t = t.Next {
		cur.Next = t.copy()
		cur = cur.Next
	}
	cur.Next = other
	return head.Next
}

/*
 * Reports an error message in the following format and exit.
 *
 * foo.c:10: x = y + 1;
 *               ^ <error message here>
 */
func verrorAt(filename string, input *[]uint8, line_no int, loc int, msg string) {
	// Find a line containing `loc`
	line := loc
	for 0 < line && (*input)[line-1] != '\n' {
		line -= 1
	}

	end := loc
	for (*input)[end] != 0 && (*input)[end] != '\n' {
		end += 1
	}

	// Print out the line.
	indent, _ := fmt.Fprintf(os.Stderr, "%s:%d: ", filename, line_no)
	fmt.Fprintf(os.Stderr, "%s", string((*input)[line:end+1]))

	// Show the error message
	pos := loc - line + indent

	fmt.Fprintf(os.Stderr, "%*s", pos, "") // print pos spaces.
	fmt.Fprintf(os.Stderr, "^ %s\n", msg)
	os.Exit(1)
}

func errorAt(loc int, msg string) {
	buf := currentFile.Contents
	line_no := 1
	for p := 0; p < loc; p += 1 {
		if (*buf)[p] == '\n' {
			line_no += 1
		}
	}

	verrorAt(currentFile.Name, buf, line_no, loc, msg)
	os.Exit(1)
}

func errorTok(tok *Token, msg string) {
	verrorAt(tok.File.Name, tok.File.Contents, tok.LineNo, tok.Location, msg)
	os.Exit(1)
}

func warnTok(tok *Token, msg string) {
	verrorAt(tok.File.Name, tok.File.Contents, tok.LineNo, tok.Location, msg)
}

func (t *Token) isEqual(s string) bool {
	return string((*t.File.Contents)[t.Location:t.Location+t.Length]) == s
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

	if 'a' <= c && c <= 'f' {
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
func readEscapedChar(src *[]uint8, p int) (int8, int) {
	buf := src
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
			c = int(uint8(c<<4) + fromHex((*buf)[p]))
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
func stringLiteralEnd(src *[]uint8, p int) int {
	buf := src
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

func readStringLiteral(src *[]uint8, start int, quote int) *Token {
	buf := src
	end := stringLiteralEnd(src, quote+1)
	str := make([]uint8, end-quote)
	var len int64 = 0

	for p := quote + 1; p < end; {
		if (*buf)[p] == '\\' {
			c, new_pos := readEscapedChar(src, p+1)
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

func readCharLiteral(src *[]uint8, start int, quote int, ty *CType) *Token {
	currentInput := src
	p := quote + 1
	if (*currentInput)[p] == 0 {
		errorAt(start, "unclosed char literal")
	}

	var c int32
	if (*currentInput)[p] == '\\' {
		_c, _p := readEscapedChar(src, p+1)
		p = _p
		c = int32(_c)
	} else {
		_c, _p := decodeUTF8(src, p)
		p = _p
		c = int32(_c)
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
	tok.Ty = ty
	return tok
}

func convertPpInt(tok *Token) bool {
	currentInput := tok.File.Contents
	p := tok.Location

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

	suffix3 := ""
	if p+3 < len(*currentInput) {
		suffix3 = string((*currentInput)[p : p+3])
	}
	suffix2 := ""
	if p+2 < len(*currentInput) {
		suffix2 = string((*currentInput)[p : p+2])
	}
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

	if p != tok.Location+tok.Length {
		return false
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

	tok.Kind = TK_NUM
	tok.Value = int64(val)
	tok.Ty = ty
	return true
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
//
// This function converts a pp-number token to a regular number token.
func convertPpNumber(tok *Token) {
	// Try to parse as an integer constant.
	if convertPpInt(tok) {
		return
	}

	src := tok.File.Contents

	// If it's not an integer, it must be a floating point constant.
	end := tok.Location + tok.Length
	if (*src)[end] == '.' {
		end += 1
	}
	for isDecimalDigit((*src)[end]) || (*src)[end] == 'p' {
		end += 1
	}
	if (*src)[end] == 'e' || (*src)[end] == 'E' {
		if (*src)[end+1] == '+' || (*src)[end+1] == '-' {
			end += 2
		} else {
			end += 1
		}

		for isDecimalDigit((*src)[end]) {
			end += 1
		}
	}

	var value float64
	if (*src)[end-1] == 'f' || (*src)[end-1] == 'F' {
		value, _ = strconv.ParseFloat(string((*src)[tok.Location:end-1]), 64)
		end -= 1
	} else {
		value, _ = strconv.ParseFloat(string((*src)[tok.Location:end]), 64)
	}

	var ty *CType
	if (*src)[end] == 'f' || (*src)[end] == 'F' {
		ty = TyFloat
		end += 1
	} else if (*src)[end] == 'l' || (*src)[end] == 'L' {
		ty = TyDouble
		end += 1
	} else {
		ty = TyDouble
	}

	if tok.Location+tok.Length != end {
		errorTok(tok, "invalid numeric constant")
	}

	tok.Kind = TK_NUM
	tok.FloatValue = value
	tok.Ty = ty
}

func convertPpTokens(tok *Token) {
	for t := tok; t != nil && t.Kind != TK_EOF; t = t.Next {
		if t.isKeyword() {
			t.Kind = TK_KEYWORD
		} else if t.Kind == TK_PP_NUM {
			convertPpNumber(t)
		}
	}
}

// Initialize line info for all tokens.
func addLineNumbers(src *[]uint8, tok *Token) {
	p := 0
	n := 1

	if p == tok.Location {
		tok.LineNo = n
		tok = tok.Next
	}
	if (*src)[p] == '\n' {
		n += 1
	}

	for ; (*src)[p] != 0; p += 1 {
		if p == tok.Location {
			tok.LineNo = n
			tok = tok.Next
		}
		if (*src)[p] == '\n' {
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
func tokenize(file *File) *Token {
	currentFile = file
	src := file.Contents

	p := 0

	head := Token{}
	cur := &head

	atBeginningOfLine = true
	hasSpace = false

	for (*src)[p] != 0 {
		// Skip line comments.
		if (*src)[p] == '/' && (*src)[p+1] == '/' {
			p += 2
			for (*src)[p] != '\n' {
				p += 1
			}
			hasSpace = true
			continue
		}

		// Character literal
		if (*src)[p] == '\'' {
			cur.Next = readCharLiteral(src, p, p, TyInt)
			cur = cur.Next
			cur.Value = int64(int8(cur.Value))
			p += cur.Length
			continue
		}

		// UTF-16 character literal
		if (*src)[p] == 'u' && (*src)[p+1] == '\'' {
			cur.Next = readCharLiteral(src, p, p+1, TyUShort)
			cur = cur.Next
			cur.Value = cur.Value & 0xFFFF
			p += cur.Length
			continue
		}

		// UTF-32 character literal
		if (*src)[p] == 'U' && (*src)[p+1] == '\'' {
			cur.Next = readCharLiteral(src, p, p+1, TyUInt)
			cur = cur.Next
			p += cur.Length
			continue
		}

		// Wide character literal
		if (*src)[p] == 'L' && (*src)[p+1] == '\'' {
			cur.Next = readCharLiteral(src, p, p+1, TyInt)
			cur = cur.Next
			p = cur.Location + cur.Length
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
			hasSpace = true
			continue
		}

		// Skip newline.
		if (*src)[p] == '\n' {
			p += 1
			atBeginningOfLine = true
			hasSpace = false
			continue
		}

		// Skip whitespace characters.
		if (*src)[p] == ' ' || (*src)[p] == '\t' || (*src)[p] == '\v' || (*src)[p] == '\f' || (*src)[p] == '\n' || (*src)[p] == '\r' {
			p += 1
			hasSpace = true
			continue
		}

		// Numeric literal
		if isDecimalDigit((*src)[p]) || ((*src)[p] == '.' && isDecimalDigit((*src)[p+1])) {
			q := p
			p += 1
			for {
				if (*src)[p] != 0 && (*src)[p+1] != 0 && ((*src)[p] == 'e' || (*src)[p] == 'E' || (*src)[p] == 'p' || (*src)[p] == 'P') && ((*src)[p+1] == '+' || (*src)[p+1] == '-') {
					p += 2
				} else if isAlphaNumber((*src)[p]) || (*src)[p] == '.' {
					p += 1
				} else {
					break
				}
			}
			cur.Next = newToken(TK_PP_NUM, q, p)
			cur = cur.Next
			continue
		}

		// String literal
		if (*src)[p] == '"' {
			cur.Next = readStringLiteral(src, p, p)
			cur = cur.Next
			p += cur.Length
			continue
		}

		// UTF-8 string literal
		if (*src)[p] == 'u' && (*src)[p+1] == '8' && (*src)[p+2] == '"' {
			cur.Next = readStringLiteral(src, p, p+2)
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
		punctLen := readPunct(src, p)
		if punctLen > 0 {
			cur.Next = newToken(TK_PUNCT, p, p+punctLen)
			cur = cur.Next
			p += cur.Length
			continue
		}

		errorAt(p, "invalid token")
	}

	cur.Next = newToken(TK_EOF, p, p)
	addLineNumbers(src, head.Next)
	return head.Next
}

// Read a punctuator token from p and returns its length.
func readPunct(src *[]uint8, p int) int {
	punctuators := []string{
		"<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=",
		"-=", "*=", "/=", "++", "--", "%=", "&=", "|=", "^=", "&&",
		"||", "<<", ">>", "##",

		// single char
		"<", ">", "=", "-", "!", "&", "|", "%", "(", ")", "[", "]", "{", "}", ";", ":",
		"#", ",", ".", "+", "-", "*", "/", "?", "~", "^", "`",
	}

	for _, punct := range punctuators {
		punctLen := len(punct)
		if string((*src)[p:p+punctLen]) == punct {
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
			return nil
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

func newFile(name string, fileNo int, contents *[]uint8) *File {
	file := &File{
		Name:     name,
		FileNo:   fileNo,
		Contents: contents,
	}
	return file
}

// Replaces \r or \r\n with \n.
func canonicalizeNewLine(src *[]uint8) {
	i := 0
	j := 0

	for (*src)[i] != 0 {
		if (*src)[i] == '\r' && (*src)[i+1] == '\n' {
			i += 2
			(*src)[j] = '\n'
			j += 1
		} else if (*src)[i] == '\r' {
			i += 1
			(*src)[j] = '\n'
			j += 1
		} else {
			(*src)[j] = (*src)[i]
			j += 1
			i += 1
		}
	}

	(*src)[j] = 0
}

func readUniversalChar(src *[]uint8, p int, len int) uint32 {
	c := uint32(0)
	for i := 0; i < len; i += 1 {
		if !isHexDigit((*src)[p+i]) {
			return 0
		}
		c = (c << 4) | uint32(fromHex((*src)[p+i]))
	}
	return c
}

// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
func convertUniversalChars(src *[]uint8) {
	p := 0
	q := 0

	for (*src)[p] != 0 {
		if (*src)[p] == '\\' && (*src)[p+1] == 'u' {
			c := readUniversalChar(src, p+2, 4)
			if c != 0 {
				p += 6
				q += encodeUTF8(src, q, c)
			} else {
				(*src)[q] = (*src)[p]
				q += 1
				p += 1
			}
		} else if (*src)[p] == '\\' && (*src)[p+1] == 'U' {
			c := readUniversalChar(src, p+2, 8)
			if c != 0 {
				p += 10
				q += encodeUTF8(src, q, c)
			} else {
				(*src)[q] = (*src)[p]
				q += 1
				p += 1
			}
		} else if (*src)[p] == '\\' {
			(*src)[q] = (*src)[p]
			q += 1
			p += 1
			(*src)[q] = (*src)[p]
			q += 1
			p += 1
		} else {
			(*src)[q] = (*src)[p]
			q += 1
			p += 1
		}
	}

	(*src)[q] = 0
}

// Removes backslashes followed by a newline.
func removeBackslashNewline(src *[]uint8) {
	i := 0
	j := 0

	// We want to keep the number of newline characters so that
	// the logical line number matches the physical one.
	// This counter maintain the number of newlines we have removed.
	n := 0

	for (*src)[i] != 0 {
		if (*src)[i] == '\\' && (*src)[i+1] == '\n' {
			i += 2
			n += 1
		} else if (*src)[i] == '\n' {
			(*src)[j] = (*src)[i]
			j += 1
			i += 1
			for ; n > 0; n -= 1 {
				(*src)[j] = '\n'
				j += 1
			}
		} else {
			(*src)[j] = (*src)[i]
			j += 1
			i += 1
		}
	}

	for ; n > 0; n -= 1 {
		(*src)[j] = '\n'
		j += 1
	}
	(*src)[j] = 0
}

func getInputFiles() []*File {
	return inputFiles
}

func tokenizeFile(path string) *Token {
	src := readFile(path)
	if src == nil {
		return nil
	}

	canonicalizeNewLine(src)
	removeBackslashNewline(src)
	convertUniversalChars(src)

	// Save the filename for assembler .file directive.
	file := newFile(path, fileNo+1, src)

	// Save the filename for assembler .file directive.
	inputFiles = append(inputFiles, file)
	fileNo += 1
	return tokenize(file)
}
