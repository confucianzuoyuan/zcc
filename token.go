package main

type TokenKind int

const (
	TK_IDENT   TokenKind = iota // Identifiers
	TK_PUNCT                    // Punctuators
	TK_KEYWORD                  // Keywords
	TK_STR                      // String literals
	TK_NUM                      // Numeric literals
	TK_PP_NUM                   // Preprocessing numbers
	TK_PMARK                    // Placermarkers
	TK_ATTR                     // GNU attribute
	TK_BATTR                    // C23 attribute
	TK_EOF                      // End-of-file markers
)

type File struct {
	Name     string
	FileNo   int
	Contents *[]int8

	// For #line directive
	DisplayFile *File
	LineDelta   int
}

type Token struct {
	Kind          TokenKind  // Token kind
	Next          *Token     // Next token
	Value         int64      // If kind is TK_NUM, its value
	FloatValue    FloatConst // If kind is TK_NUM, its value
	Location      int        // Token location
	Length        int        // Token length
	Ty            *CType     // Used if TK_NUM or TK_STR
	StringLiteral []int8     // String literal contents including terminating '\0'

	File              *File // Source location
	DisplayLineNo     int
	DisplayFileNo     int
	LineNo            int    // Line number
	AtBeginningOfLine bool   // True if this token is at beginning of line
	HasSpace          bool   // True if this token follows a space character
	DontExpand        bool   // True if a macro token is encountered during the macro's expansion
	Origin            *Token // If this is expanded from a macro, the original token
	GuardFile         string // The path of a potentially include-guarded file
	AttrNext          *Token
}

func (tok *Token) getIdent() string {
	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected an identifier")
	}
	return B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
}

func (tok *Token) getText() string {
	return B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
}
