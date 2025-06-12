// This file implements the C preprocessor.
//
// The preprocessor takes a list of tokens as an input and returns a
// new list of tokens as an output.
//
// The preprocessing language is designed in such a way that that's
// guaranteed to stop even if there is a recursive macro.
// Informally speaking, a macro is applied only once for each token.
// That is, if a macro token T appears in a result of direct or
// indirect macro expansion of T, T won't be expanded any further.
// For example, if T is defined as U, and U is defined as T, then
// token T is expanded to U and then to T and the macro expansion
// stops at that point.
//
// To achieve the above behavior, we attach for each token a set of
// macro names from which the token is expanded. The set is called
// "hideset". Hideset is initially empty, and every time we expand a
// macro, the macro name is added to the resulting tokens' hidesets.
//
// The above macro expansion algorithm is explained in this document
// written by Dave Prossor, which is used as a basis for the
// standard's wording:
// https://github.com/rui314/chibicc/wiki/cpp.algo.pdf

package main

import (
	"fmt"
	"path/filepath"
)

type CondInclCtx int

const (
	IN_THEN CondInclCtx = iota
	IN_ELIF
	IN_ELSE
)

// `#if` can be nested, so we use a stack to manage nested `#if`s.
type CondIncl struct {
	Next     *CondIncl
	Tok      *Token
	Included bool
	Ctx      CondInclCtx
}

type Macro struct {
	Next      *Macro
	Name      string
	IsObjlike bool
	Params    *MacroParam
	Body      *Token
	Deleted   bool
}

type MacroParam struct {
	Next *MacroParam
	Name string
}

type MacroArg struct {
	Next *MacroArg
	Name string
	Tok  *Token
}

type Hideset struct {
	Next *Hideset
	Name string
}

var condIncl *CondIncl
var macros *Macro

func newNumberToken(val int, tmpl *Token) *Token {
	buf := []uint8(fmt.Sprintf("%d\n", val))
	buf = append(buf, 0)
	return tokenize(newFile(tmpl.File.Name, tmpl.File.FileNo, &buf))
}

func readConstExpr(rest **Token, tok *Token) *Token {
	tok = copyLine(rest, tok)

	head := Token{}
	cur := &head

	for tok.Kind != TK_EOF {
		// "defined(foo)" or "defined foo" becomes "1" if macro "foo"
		// is defined. Otherwise "0".
		if tok.isEqual("defined") {
			start := tok
			hasParen := consume(&tok, tok.Next, "(")

			if tok.Kind != TK_IDENT {
				errorTok(start, "macro name must be an identifier")
			}

			m := findMacro(tok)
			tok = tok.Next

			if hasParen {
				tok = skip(tok, ")")
			}

			if m != nil {
				cur.Next = newNumberToken(1, start)
				cur = cur.Next
			} else {
				cur.Next = newNumberToken(0, start)
				cur = cur.Next
			}
			continue
		}

		cur.Next = tok
		cur = cur.Next
		tok = tok.Next
	}

	cur.Next = tok
	return head.Next
}

func (t *Token) newEOF() *Token {
	newToken := t.copy()
	newToken.Kind = TK_EOF
	newToken.Length = 0
	return newToken
}

func skipCondIncl2(tok *Token) *Token {
	for tok != nil && tok.Kind != TK_EOF {
		if tok.isHash() && (tok.Next.isEqual("if") || tok.Next.isEqual("ifdef") || tok.Next.isEqual("ifndef")) {
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

// Skip until next `#else` or `#endif` or `#elif`.
// Nested `#if` and `#endif` are skipped.
func skipCondIncl(tok *Token) *Token {
	for tok != nil && tok.Kind != TK_EOF {
		if tok.isHash() && (tok.Next.isEqual("if") || tok.Next.isEqual("ifdef") || tok.Next.isEqual("ifndef")) {
			tok = skipCondIncl2(tok.Next.Next)
			continue
		}
		if tok.isHash() && (tok.Next.isEqual("endif") || tok.Next.isEqual("else") || tok.Next.isEqual("elif")) {
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
	expr := readConstExpr(rest, tok.Next)
	expr = preprocess2(expr)

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

// Double-quote a given string and returns it.
func quoteString(s string) []uint8 {
	buf := []uint8{}
	buf = append(buf, '"')
	for _, c := range []uint8(s) {
		if c == '\\' || c == '"' {
			buf = append(buf, '\\')
		}
		buf = append(buf, c)
	}
	buf = append(buf, '"')

	return buf
}

func newStringToken(s string, tmpl *Token) *Token {
	buf := quoteString(s)
	buf = append(buf, 0)
	f := newFile(tmpl.File.Name, tmpl.File.FileNo, &buf)
	return tokenize(f)
}

// Concatenates all tokens in `tok` and returns a new string.
func (tok *Token) joinTokens() []uint8 {
	buf := []uint8{}

	for t := tok; t != nil && t.Kind != TK_EOF; t = t.Next {
		if t != tok && t.HasSpace {
			buf = append(buf, ' ')
		}
		buf = append(buf, (*t.File.Contents)[t.Location:t.Location+t.Length]...)
	}
	return buf
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
func stringize(hash *Token, arg *Token) *Token {
	// Create a new string token. We need to set some value to its
	// source location for error reporting function, so we use a macro
	// name token as a template.
	buf := arg.joinTokens()
	return newStringToken(string(buf), hash)
}

func findMacro(tok *Token) *Macro {
	if tok.Kind != TK_IDENT {
		return nil
	}

	for m := macros; m != nil; m = m.Next {
		if tok.isEqual(m.Name) {
			if m.Deleted {
				return nil
			} else {
				return m
			}
		}
	}

	return nil
}

func addMacro(name string, isObjlike bool, body *Token) *Macro {
	m := &Macro{
		Next:      macros,
		Name:      name,
		Body:      body,
		IsObjlike: isObjlike,
	}
	macros = m
	return m
}

func readMacroParams(rest **Token, tok *Token) *MacroParam {
	head := MacroParam{}
	cur := &head

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}

		if tok.Kind != TK_IDENT {
			errorTok(tok, "expected an identifier")
		}

		m := &MacroParam{
			Name: string((*tok.File.Contents)[tok.Location : tok.Location+tok.Length]),
		}

		cur.Next = m
		cur = cur.Next
		tok = tok.Next
	}

	*rest = tok.Next
	return head.Next
}

func readMacroDefinition(rest **Token, tok *Token) {
	if tok.Kind != TK_IDENT {
		errorTok(tok, "macro name must be an identifier")
	}
	name := string((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
	tok = tok.Next

	if !tok.HasSpace && tok.isEqual("(") {
		// Function-like macro
		params := readMacroParams(&tok, tok.Next)
		m := addMacro(name, false, copyLine(rest, tok))
		m.Params = params
	} else {
		// Object-like macro
		addMacro(name, true, copyLine(rest, tok))
	}
}

func readMacroArgOne(rest **Token, tok *Token) *MacroArg {
	head := Token{}
	cur := &head
	level := 0

	for level > 0 || (!tok.isEqual(",") && !tok.isEqual(")")) {
		if tok.Kind == TK_EOF {
			errorTok(tok, "premature end of input")
		}

		if tok.isEqual("(") {
			level += 1
		} else if tok.isEqual(")") {
			level -= 1
		}
		cur.Next = tok.copy()
		cur = cur.Next
		tok = tok.Next
	}

	cur.Next = tok.newEOF()

	arg := &MacroArg{}
	arg.Tok = head.Next
	*rest = tok
	return arg
}

func readMacroArgs(rest **Token, tok *Token, params *MacroParam) *MacroArg {
	tok = tok.Next.Next

	head := MacroArg{}
	cur := &head

	pp := params
	for ; pp != nil; pp = pp.Next {
		if cur != &head {
			tok = skip(tok, ",")
		}
		cur.Next = readMacroArgOne(&tok, tok)
		cur = cur.Next
		cur.Name = pp.Name
	}

	skip(tok, ")")
	*rest = tok
	return head.Next
}

func findArg(args *MacroArg, tok *Token) *MacroArg {
	for ap := args; ap != nil; ap = ap.Next {
		if ap.Name == string((*tok.File.Contents)[tok.Location:tok.Location+tok.Length]) {
			return ap
		}
	}

	return nil
}

// Concatenate two tokens to create a new token.
func paste(lhs *Token, rhs *Token) *Token {
	// Paste the two tokens.
	lhsString := string((*lhs.File.Contents)[lhs.Location : lhs.Location+lhs.Length])
	rhsString := string((*rhs.File.Contents)[rhs.Location : rhs.Location+rhs.Length])
	buf := []uint8(lhsString + rhsString)
	buf = append(buf, 0)

	// Tokenize the resulting string
	tok := tokenize(newFile(lhs.File.Name, lhs.File.FileNo, &buf))
	if tok.Next.Kind != TK_EOF {
		errorTok(lhs, fmt.Sprintf("pasting forms '%s', an invalid token", string(buf)))
	}
	return tok
}

// Replace func-like macro parameters with given arguments.
func subst(tok *Token, args *MacroArg) *Token {
	head := Token{}
	cur := &head

	for tok != nil && tok.Kind != TK_EOF {
		// "#" followed by a parameter is replaced with stringized actuals.
		if tok.isEqual("#") {
			arg := findArg(args, tok.Next)
			if arg == nil {
				errorTok(tok.Next, "'#' is not followed by a macro parameter")
			}
			cur.Next = stringize(tok, arg.Tok)
			cur = cur.Next
			tok = tok.Next.Next
			continue
		}

		if tok.isEqual("##") {
			if cur == &head {
				errorTok(tok, "'##' cannot appear at start of macro expansion")
			}

			if tok.Next.Kind == TK_EOF {
				errorTok(tok, "'##' cannot appear at end of macro expansion")
			}

			arg := findArg(args, tok.Next)
			if arg != nil {
				if arg.Tok.Kind != TK_EOF {
					*cur = *paste(cur, arg.Tok)
					for t := arg.Tok.Next; t.Kind != TK_EOF; t = t.Next {
						cur.Next = t.copy()
						cur = cur.Next
					}
				}
				tok = tok.Next.Next
				continue
			}

			*cur = *paste(cur, tok.Next)
			tok = tok.Next.Next
			continue
		}

		arg := findArg(args, tok)

		if arg != nil && tok.Next.isEqual("##") {
			rhs := tok.Next.Next

			if arg.Tok.Kind == TK_EOF {
				arg2 := findArg(args, rhs)
				if arg2 != nil {
					for t := arg2.Tok; t.Kind != TK_EOF; t = t.Next {
						cur.Next = t.copy()
						cur = cur.Next
					}
				} else {
					cur.Next = rhs.copy()
					cur = cur.Next
				}
				tok = rhs.Next
				continue
			}

			for t := arg.Tok; t.Kind != TK_EOF; t = t.Next {
				cur.Next = t.copy()
				cur = cur.Next
			}

			tok = tok.Next
			continue
		}

		// Handle a macro token. Macro arguments are completely macro-expanded
		// before they are substituted into a macro body.
		if arg != nil {
			t := preprocess2(arg.Tok)
			for ; t.Kind != TK_EOF; t = t.Next {
				cur.Next = t.copy()
				cur = cur.Next
			}
			tok = tok.Next
			continue
		}

		// Handle a non-macro token.
		cur.Next = tok.copy()
		cur = cur.Next
		tok = tok.Next
		continue
	}

	cur.Next = tok
	return head.Next
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
func expandMacro(rest **Token, tok *Token) bool {
	if tok.Hideset.contains(string((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])) {
		return false
	}

	m := findMacro(tok)
	if m == nil {
		return false
	}

	// Object-like macro application
	if m.IsObjlike {
		hs := tok.Hideset.union(newHideset(m.Name))
		body := addHideset(m.Body, hs)
		*rest = body.append(tok.Next)
		return true
	}

	// If a funclike macro token is not followed by an argument list,
	// treat it as a normal identifier.
	if !tok.Next.isEqual("(") {
		return false
	}

	// Function-like macro application
	macroToken := tok
	args := readMacroArgs(&tok, tok, m.Params)
	rparen := tok

	// Tokens that consist a func-like macro invocation may have different
	// hidesets, and if that's the case, it's not clear what the hideset
	// for the new tokens should be. We take the interesection of the
	// macro token and the closing parenthesis and use it as a new hideset
	// as explained in the Dave Prossor's algorithm.
	hs := macroToken.Hideset.intersection(rparen.Hideset)
	hs = hs.union(newHideset(m.Name))

	body := subst(m.Body, args)
	body = addHideset(body, hs)
	*rest = body.append(tok.Next)
	return true
}

func newHideset(name string) *Hideset {
	hs := &Hideset{
		Name: name,
	}
	return hs
}

func (hs1 *Hideset) union(hs2 *Hideset) *Hideset {
	head := Hideset{}
	cur := &head

	for ; hs1 != nil; hs1 = hs1.Next {
		cur.Next = newHideset(hs1.Name)
		cur = cur.Next
	}
	cur.Next = hs2
	return head.Next
}

func (hs *Hideset) contains(name string) bool {
	for ; hs != nil; hs = hs.Next {
		if hs.Name == name {
			return true
		}
	}
	return false
}

func addHideset(tok *Token, hs *Hideset) *Token {
	head := Token{}
	cur := &head

	for ; tok != nil; tok = tok.Next {
		t := tok.copy()
		t.Hideset = t.Hideset.union(hs)
		cur.Next = t
		cur = cur.Next
	}

	return head.Next
}

func (hs1 *Hideset) intersection(hs2 *Hideset) *Hideset {
	head := Hideset{}
	cur := &head

	for ; hs1 != nil; hs1 = hs1.Next {
		if hs2.contains(hs1.Name) {
			cur.Next = newHideset(hs1.Name)
			cur = cur.Next
		}
	}
	return head.Next
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
		// If it is a macro, expand it.
		if expandMacro(&tok, tok) {
			continue
		}

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
			tok = tok2.append(tok)

			continue
		}

		if tok.isEqual("define") {
			readMacroDefinition(&tok, tok.Next)
			continue
		}

		if tok.isEqual("undef") {
			tok = tok.Next
			if tok.Kind != TK_IDENT {
				errorTok(tok, "macro name must be an identifier")
			}
			name := string((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
			tok = skipLine(tok.Next)

			m := addMacro(name, true, nil)
			m.Deleted = true
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

		if tok.isEqual("ifdef") {
			defined := findMacro(tok.Next)
			if defined != nil {
				pushCondIncl(tok, true)
			} else {
				pushCondIncl(tok, false)
			}
			tok = skipLine(tok.Next.Next)
			if defined == nil {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.isEqual("ifndef") {
			defined := findMacro(tok.Next)
			if defined == nil {
				pushCondIncl(tok, true)
			} else {
				pushCondIncl(tok, false)
			}
			tok = skipLine(tok.Next.Next)
			if defined != nil {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.isEqual("elif") {
			if condIncl == nil || condIncl.Ctx == IN_ELSE {
				errorTok(start, "stray #elif")
			}
			condIncl.Ctx = IN_ELIF

			if !condIncl.Included && evalConstExpr(&tok, tok) != 0 {
				condIncl.Included = true
			} else {
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
