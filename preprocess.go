package main

import "path/filepath"

func (t *Token) isHash() bool {
	return t.AtBeginningOfLine && t.isEqual("#")
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
func preprocess2(tok *Token) *Token {
	head := Token{}
	cur := &head

	for tok.Kind != TK_EOF {
		// Pass through if it is not a "#".
		if !tok.isHash() {
			cur.Next = tok
			cur = cur.Next
			tok = tok.Next
			continue
		}

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
			path := filepath.Dir(tok.File.Name) + "/" + s
			tok2 := tokenizeFile(path)
			if tok2 == nil {
				errorTok(tok, "error +")
			}
			tok = tok2.append(tok.Next)

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
	convertKeywords(tok)
	return tok
}
