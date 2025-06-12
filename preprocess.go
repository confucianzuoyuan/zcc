package main

import "path/filepath"

type CondInclCtx int

const (
	IN_THEN CondInclCtx = iota
	IN_ELSE
)

// `#if` can be nested, so we use a stack to manage nested `#if`s.
type CondIncl struct {
	Next     *CondIncl
	Tok      *Token
	Included bool
	Ctx      CondInclCtx
}

var condIncl *CondIncl

func (t *Token) newEOF() *Token {
	newToken := t.copy()
	newToken.Kind = TK_EOF
	newToken.Length = 0
	return newToken
}

func skipCondIncl2(tok *Token) *Token {
	for tok != nil && tok.Kind != TK_EOF {
		if tok.isHash() && tok.Next.isEqual("if") {
			tok = skipCondIncl2(tok.Next.Next)
			continue
		}
		if tok.isHash() && tok.Next.isEqual("endif") {
			return tok.Next.Next
		}

		tok = tok.Next
	}

	return tok
}

// Skip until next `#else` or `#endif`.
// Nested `#if` and `#endif` are skipped.
func skipCondIncl(tok *Token) *Token {
	for tok != nil && tok.Kind != TK_EOF {
		if tok.isHash() && tok.Next.isEqual("if") {
			tok = skipCondIncl2(tok.Next.Next)
			continue
		}
		if tok.isHash() && (tok.Next.isEqual("endif") || tok.Next.isEqual("else")) {
			break
		}
		tok = tok.Next
	}

	return tok
}

// Copy all tokens until the next newline, terminate them with
// an EOF token and then returns them. This function is used to
// create a new list of tokens for `#if` arguments.
func copyLine(rest **Token, tok *Token) *Token {
	head := Token{}
	cur := &head

	for ; tok != nil && !tok.AtBeginningOfLine; tok = tok.Next {
		cur.Next = tok.copy()
		cur = cur.Next
	}

	cur.Next = tok.newEOF()
	*rest = tok
	return head.Next
}

// Read and evaluate a constant expression.
func evalConstExpr(rest **Token, tok *Token) int64 {
	start := tok
	expr := copyLine(rest, tok.Next)

	if expr.Kind == TK_EOF {
		errorTok(start, "no expressions")
	}

	var rest2 *Token
	val := constExpr(&rest2, expr)
	if rest2.Kind != TK_EOF {
		errorTok(rest2, "extra token")
	}

	return val
}

func pushCondIncl(tok *Token, included bool) *CondIncl {
	ci := &CondIncl{
		Next:     condIncl,
		Tok:      tok,
		Ctx:      IN_THEN,
		Included: included,
	}
	condIncl = ci
	return ci
}

func (t *Token) isHash() bool {
	return t.AtBeginningOfLine && t.isEqual("#")
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
func skipLine(tok *Token) *Token {
	if tok.AtBeginningOfLine {
		return tok
	}

	warnTok(tok, "extra token")
	for tok.AtBeginningOfLine {
		tok = tok.Next
	}
	return tok
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
func preprocess2(tok *Token) *Token {
	head := Token{}
	cur := &head

	for tok != nil && tok.Kind != TK_EOF {
		// Pass through if it is not a "#".
		if !tok.isHash() {
			cur.Next = tok
			cur = cur.Next
			tok = tok.Next
			continue
		}

		start := tok
		tok = tok.Next

		if tok.isEqual("include") {
			tok = tok.Next

			if tok.Kind != TK_STR {
				errorTok(tok, "expected a filename")
			}

			// 去掉字符串字面量末尾的"\x00"
			s := tok.StringLiteral
			if len(s) > 0 && s[len(s)-1] == 0 {
				s = s[:len(s)-1]
			}
			path := ""
			if s[0] == '/' {
				path = s
			} else {
				path = filepath.Dir(tok.File.Name) + "/" + s
			}

			tok2 := tokenizeFile(path)
			if tok2 == nil {
				errorTok(tok, "error +")
			}

			tok = skipLine(tok.Next)
			tok = tok2.append(tok.Next)

			continue
		}

		if tok.isEqual("if") {
			val := evalConstExpr(&tok, tok)
			var included bool = true
			if val == 0 {
				included = false
			}
			pushCondIncl(start, included)
			if val == 0 {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.isEqual("else") {
			if condIncl == nil || condIncl.Ctx == IN_ELSE {
				errorTok(start, "stray #else")
			}
			condIncl.Ctx = IN_ELSE
			tok = skipLine(tok.Next)

			if condIncl.Included {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.isEqual("endif") {
			if condIncl == nil {
				errorTok(start, "stray #endif")
			}
			condIncl = condIncl.Next
			tok = skipLine(tok.Next)
			continue
		}

		// `#`-only line is legal. It's called a null directive.
		if tok.AtBeginningOfLine {
			continue
		}

		errorTok(tok, "invalid preprocessor directive")
	}

	cur.Next = tok
	return head.Next
}

// Entry point function of the preprocessor.
func preprocess(tok *Token) *Token {
	tok = preprocess2(tok)
	if condIncl != nil {
		errorTok(condIncl.Tok, "unterminated conditional directive")
	}
	convertKeywords(tok)
	return tok
}
