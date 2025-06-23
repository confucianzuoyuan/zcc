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
	"os"
	"path/filepath"
	"time"
)

type StringKind int

const (
	STR_NONE StringKind = iota
	STR_UTF8
	STR_UTF16
	STR_UTF32
	STR_WIDE
)

func (tok *Token) getStringKind() StringKind {
	s := B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
	if s == "u8" {
		return STR_UTF8
	}

	switch s[0] {
	case '"':
		return STR_NONE
	case 'u':
		return STR_UTF16
	case 'U':
		return STR_UTF32
	case 'L':
		return STR_WIDE
	}
	panic("unreachable")
}

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

type MacroHandlerFn func(*Token) *Token

type Macro struct {
	Name       string
	IsObjlike  bool
	Params     *MacroParam
	VaArgsName string
	Body       *Token
	Handler    MacroHandlerFn
}

type MacroParam struct {
	Next *MacroParam
	Name string
}

type MacroArg struct {
	Next     *MacroArg
	Name     string
	IsVaArgs bool
	Tok      *Token
}

type Hideset struct {
	Next *Hideset
	Name string
}

var condIncl *CondIncl
var macros = make(map[string]*Macro)

func U82I8(arr []uint8) []int8 {
	res := []int8{}
	for _, b := range arr {
		res = append(res, int8(b))
	}
	return res
}

func newNumberToken(val int, tmpl *Token) *Token {
	buf := []uint8(fmt.Sprintf("%d\n", val))
	buf = append(buf, 0)
	newBuf := U82I8(buf)
	return tokenize(newFile(tmpl.File.Name, tmpl.File.FileNo, &newBuf), nil)
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

// Read #line arguments
func readLineMarker(rest **Token, tok *Token) {
	start := tok
	tok = preprocess(copyLine(rest, tok))

	if tok.Kind != TK_NUM || tok.Ty.Kind != TY_INT {
		errorTok(tok, "invalid line marker")
	}
	start.File.LineDelta = int(tok.Value) - start.LineNo

	tok = tok.Next
	if tok.Kind == TK_EOF {
		return
	}

	if tok.Kind != TK_STR {
		errorTok(tok, "filename expected")
	}
	start.File.DisplayName = B2S(tok.StringLiteral[:len(tok.StringLiteral)-1])
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

	// [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
	// we replace remaining non-macro identifiers with "0" before
	// evaluating a constant expression. For example, `#if foo` is
	// equivalent to `#if 0` if foo is not defined.
	for t := expr; t.Kind != TK_EOF; t = t.Next {
		if t.Kind == TK_IDENT {
			next := t.Next
			*t = *newNumberToken(0, t)
			t.Next = next
		}
	}

	// Convert pp-numbers to regular numbers
	convertPpTokens(expr)

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
	newBuf := U82I8(buf)
	f := newFile(tmpl.File.Name, tmpl.File.FileNo, &newBuf)
	return tokenize(f, nil)
}

// Concatenates all tokens in `tok` and returns a new string.
func (tok *Token) joinTokens(end *Token) []int8 {
	buf := []int8{}

	for t := tok; t != end && t.Kind != TK_EOF; t = t.Next {
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
	buf := arg.joinTokens(nil)
	return newStringToken(B2S(buf), hash)
}

// Read an #include argument.
func readIncludeFilename(rest **Token, tok *Token, isDoubleQuote *bool) string {
	// Pattern 1: #include "foo.h"
	if tok.Kind == TK_STR {
		// A double-quoted filename for #include is a special kind of
		// token, and we don't want to interpret any escape sequences in it.
		// For example, "\f" in "C:\foo" is not a formfeed character but
		// just two non-control characters, backslash and f.
		// So we don't want to use token->str.
		*isDoubleQuote = true
		*rest = skipLine(tok.Next)
		return B2S((*tok.File.Contents)[tok.Location+1 : tok.Location+tok.Length-1])
	}

	// Pattern 2: #include <foo.h>
	if tok.isEqual("<") {
		// Reconstruct a filename from a sequence of tokens between
		// "<" and ">".
		start := tok

		// Find closing ">".
		for ; !tok.isEqual(">"); tok = tok.Next {
			if tok.AtBeginningOfLine || tok.Kind == TK_EOF {
				errorTok(tok, "expected '>'")
			}
		}

		*isDoubleQuote = false
		*rest = skipLine(tok.Next)
		return B2S(start.Next.joinTokens(tok))
	}

	// Pattern 3: #include FOO
	// In this case FOO must be macro-expanded to either
	// a single string token or a sequence of "<" ... ">".
	if tok.Kind == TK_IDENT {
		tok2 := preprocess2(copyLine(rest, tok))
		return readIncludeFilename(&tok2, tok2, isDoubleQuote)
	}

	errorTok(tok, "expected a filename")
	panic("unreachable")
}

var PragmaOnce = make(map[string]int)
var IncludeNextIdx int

func searchIncludeNext(filename string) string {
	for ; IncludeNextIdx < len(includePaths); IncludeNextIdx++ {
		path := includePaths[IncludeNextIdx] + "/" + filename
		if fileExists(path) {
			return path
		}
	}

	return ""
}

func includeFile(tok *Token, path string, filenameToken *Token) *Token {
	// Check for "#pragma once"
	if _, ok := PragmaOnce[path]; ok {
		return tok
	}

	var end *Token = nil
	start := tokenizeFile(path, &end)
	if start == nil {
		errorTok(filenameToken, "cannot open file")
	}
	if end == nil {
		return tok
	}

	end.Next = tok
	return start
}

func findMacro(tok *Token) *Macro {
	if tok.Kind != TK_IDENT {
		return nil
	}

	name := B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
	return macros[name]
}

func addMacro(name string, isObjlike bool, body *Token) *Macro {
	m := &Macro{
		Name:      name,
		Body:      body,
		IsObjlike: isObjlike,
	}
	macros[name] = m
	return m
}

func readMacroParams(rest **Token, tok *Token, vaArgsName *string) *MacroParam {
	head := MacroParam{}
	cur := &head

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}

		if tok.isEqual("...") {
			*vaArgsName = "__VA_ARGS__"
			*rest = skip(tok.Next, ")")
			return head.Next
		}

		if tok.Kind != TK_IDENT {
			errorTok(tok, "expected an identifier")
		}

		if tok.Next.isEqual("...") {
			*vaArgsName = B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
			*rest = skip(tok.Next.Next, ")")
			return head.Next
		}

		m := &MacroParam{
			Name: B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length]),
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
	name := B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
	tok = tok.Next

	if !tok.HasSpace && tok.isEqual("(") {
		// Function-like macro
		vaArgsName := ""
		params := readMacroParams(&tok, tok.Next, &vaArgsName)
		m := addMacro(name, false, copyLine(rest, tok))
		m.Params = params
		m.VaArgsName = vaArgsName
	} else {
		// Object-like macro
		addMacro(name, true, copyLine(rest, tok))
	}
}

func readMacroArgOne(rest **Token, tok *Token, readRest bool) *MacroArg {
	head := Token{}
	cur := &head
	level := 0

	for {
		if level == 0 && tok.isEqual(")") {
			break
		}
		if level == 0 && !readRest && tok.isEqual(",") {
			break
		}

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

func readMacroArgs(rest **Token, tok *Token, params *MacroParam, vaArgsName string) *MacroArg {
	tok = tok.Next.Next

	head := MacroArg{}
	cur := &head

	pp := params
	for ; pp != nil; pp = pp.Next {
		if cur != &head {
			tok = skip(tok, ",")
		}
		cur.Next = readMacroArgOne(&tok, tok, false)
		cur = cur.Next
		cur.Name = pp.Name
	}

	if vaArgsName != "" {
		var arg *MacroArg
		if tok.isEqual(")") {
			arg = &MacroArg{
				Tok: tok.newEOF(),
			}
		} else {
			if pp != params {
				tok = skip(tok, ",")
			}
			arg = readMacroArgOne(&tok, tok, true)
		}
		arg.Name = vaArgsName
		arg.IsVaArgs = true
		cur.Next = arg
	}

	skip(tok, ")")
	*rest = tok
	return head.Next
}

func findArg(args *MacroArg, tok *Token) *MacroArg {
	for ap := args; ap != nil; ap = ap.Next {
		if ap.Name == B2S((*tok.File.Contents)[tok.Location:tok.Location+tok.Length]) {
			return ap
		}
	}

	return nil
}

// Concatenate two tokens to create a new token.
func paste(lhs *Token, rhs *Token) *Token {
	// Paste the two tokens.
	lhsString := B2S((*lhs.File.Contents)[lhs.Location : lhs.Location+lhs.Length])
	rhsString := B2S((*rhs.File.Contents)[rhs.Location : rhs.Location+rhs.Length])
	buf := []uint8(lhsString + rhsString)
	buf = append(buf, 0)
	newBuf := U82I8(buf)

	// Tokenize the resulting string
	tok := tokenize(newFile(lhs.File.Name, lhs.File.FileNo, &newBuf), nil)
	if tok.Next.Kind != TK_EOF {
		errorTok(lhs, fmt.Sprintf("pasting forms '%s', an invalid token", string(buf)))
	}
	return tok
}

func hasVarargs(args *MacroArg) bool {
	for ap := args; ap != nil; ap = ap.Next {
		if ap.Name == "__VA_ARGS__" {
			return ap.Tok.Kind != TK_EOF
		}
	}

	return false
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

		// [GNU] If __VA_ARG__ is empty, `,##__VA_ARGS__` is expanded
		// to the empty token list. Otherwise, its expaned to `,` and
		// __VA_ARGS__.
		if tok.isEqual(",") && tok.Next.isEqual("##") {
			arg := findArg(args, tok.Next.Next)
			if arg != nil && arg.IsVaArgs {
				if arg.Tok.Kind == TK_EOF {
					tok = tok.Next.Next.Next
				} else {
					cur.Next = tok.copy()
					cur = cur.Next
					tok = tok.Next.Next
				}

				continue
			}
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

		// If __VA_ARG__ is empty, __VA_OPT__(x) is expanded to the
		// empty token list. Otherwise, __VA_OPT__(x) is expanded to x.
		if tok.isEqual("__VA_OPT__") && tok.Next.isEqual("(") {
			arg := readMacroArgOne(&tok, tok.Next.Next, true)
			if hasVarargs(args) {
				for t := arg.Tok; t.Kind != TK_EOF; t = t.Next {
					cur.Next = t
					cur = cur.Next
				}
			}
			tok = skip(tok, ")")
			continue
		}

		// Handle a macro token. Macro arguments are completely macro-expanded
		// before they are substituted into a macro body.
		if arg != nil {
			t := preprocess2(arg.Tok)
			t.AtBeginningOfLine = tok.AtBeginningOfLine
			t.HasSpace = tok.HasSpace
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
	if tok.Hideset.contains(B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])) {
		return false
	}

	m := findMacro(tok)
	if m == nil {
		return false
	}

	// Built-in dynamic macro application such as __LINE__
	if m.Handler != nil {
		*rest = m.Handler(tok)
		(*rest).Next = tok.Next
		return true
	}

	// Object-like macro application
	if m.IsObjlike {
		hs := tok.Hideset.union(newHideset(m.Name))
		body := addHideset(m.Body, hs)
		for t := body; t.Kind != TK_EOF; t = t.Next {
			t.Origin = tok
		}
		*rest = body.append(tok.Next)
		(*rest).AtBeginningOfLine = tok.AtBeginningOfLine
		(*rest).HasSpace = tok.HasSpace
		return true
	}

	// If a funclike macro token is not followed by an argument list,
	// treat it as a normal identifier.
	if !tok.Next.isEqual("(") {
		return false
	}

	// Function-like macro application
	macroToken := tok
	args := readMacroArgs(&tok, tok, m.Params, m.VaArgsName)
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
	for t := body; t.Kind != TK_EOF; t = t.Next {
		t.Origin = macroToken
	}
	*rest = body.append(tok.Next)
	(*rest).AtBeginningOfLine = macroToken.AtBeginningOfLine
	(*rest).HasSpace = macroToken.HasSpace
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

var Cache = make(map[string]string)

func searchIncludePaths(filename string) string {
	if filename[0] == '/' {
		return filename
	}

	cached, ok := Cache[filename]
	if ok {
		return cached
	}

	// Search a file from the include paths.
	for i, f := range includePaths {
		path := f + "/" + filename
		if !fileExists(path) {
			continue
		}
		Cache[filename] = path
		IncludeNextIdx = i + 1
		return path
	}

	return ""
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
			tok.LineDelta = tok.File.LineDelta
			tok.Filename = tok.File.DisplayName
			cur.Next = tok
			cur = cur.Next
			tok = tok.Next
			continue
		}

		start := tok
		tok = tok.Next

		if tok.isEqual("include") {
			isDoubleQuote := false
			filename := readIncludeFilename(&tok, tok.Next, &isDoubleQuote)

			// remove '\x00' in the end
			if len(filename) > 0 && filename[len(filename)-1] == 0 {
				filename = filename[:len(filename)-1]
			}

			if filename[0] != '/' && isDoubleQuote {
				path := filepath.Dir(start.File.Name) + "/" + filename
				_, err := os.Stat(path)
				if err == nil {
					tok = includeFile(tok, path, start.Next.Next)
					continue
				}
			}

			path := searchIncludePaths(filename)
			if path != "" {
				tok = includeFile(tok, path, start.Next.Next)
			} else {
				tok = includeFile(tok, filename, start.Next.Next)
			}
			continue
		}

		if tok.isEqual("include_next") {
			ignore := false
			filename := readIncludeFilename(&tok, tok.Next, &ignore)
			path := searchIncludeNext(filename)
			if path != "" {
				tok = includeFile(tok, path, start.Next.Next)
			} else {
				tok = includeFile(tok, filename, start.Next.Next)
			}

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
			name := B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
			undefMacro(name)
			tok = skipLine(tok.Next)

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

		if tok.isEqual("line") {
			readLineMarker(&tok, tok.Next)
			continue
		}

		if tok.Kind == TK_PP_NUM {
			readLineMarker(&tok, tok)
			continue
		}

		if tok.isEqual("pragma") && tok.Next.isEqual("once") {
			PragmaOnce[tok.File.Name] = 1
			tok = skipLine(tok.Next.Next)
			continue
		}

		if tok.isEqual("pragma") {
			tok = tok.Next
			for !tok.AtBeginningOfLine {
				tok = tok.Next
			}
			continue
		}

		if tok.isEqual("error") {
			errorTok(tok, "error")
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

func undefMacro(name string) {
	delete(macros, name)
}

func defineMacro(name string, buf string) {
	b := []uint8(buf)
	b = append(b, 0)
	newB := U82I8(b)
	tok := tokenize(newFile("<built-in>", 1, &newB), nil)
	addMacro(name, true, tok)
}

func addBuiltin(name string, fn MacroHandlerFn) *Macro {
	m := addMacro(name, true, nil)
	m.Handler = fn
	return m
}

func fileMacro(tmpl *Token) *Token {
	for tmpl.Origin != nil {
		tmpl = tmpl.Next
	}
	return newStringToken(tmpl.File.DisplayName, tmpl)
}

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
func timestampMacro(tmpl *Token) *Token {
	fi, err := os.Stat(tmpl.File.Name)
	if err != nil {
		return newStringToken("??? ??? ?? ??:??:?? ????", tmpl)
	}
	modTime := fi.ModTime()

	// 格式化成类似 "Fri Jul 24 01:32:50 2020"
	// Go 的 time.Format 需要用参考时间 Mon Jan 2 15:04:05 MST 2006
	formatted := modTime.Format("Mon Jan 02 15:04:05 2006")

	return newStringToken(formatted, tmpl)
}

func baseFileMacro(tmpl *Token) *Token {
	return newStringToken(baseFile, tmpl)
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
// formatDate 格式化时间为类似 __DATE__ 的字符串，带双引号，比如 "May 17 2020"
func formatDate(t time.Time) string {
	months := []string{
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	}

	monthStr := months[t.Month()-1] // Go的Month从1开始
	day := t.Day()
	year := t.Year()

	return fmt.Sprintf("\"%s %2d %d\"", monthStr, day, year)
}

// __TIME__ is expanded to the current time, e.g. "13:34:03".
// formatTime 格式化时间为 "HH:MM:SS" 格式，带双引号
func formatTime(t time.Time) string {
	return fmt.Sprintf("\"%02d:%02d:%02d\"", t.Hour(), t.Minute(), t.Second())
}

func lineMacro(tmpl *Token) *Token {
	for tmpl.Origin != nil {
		tmpl = tmpl.Next
	}
	i := tmpl.LineNo + tmpl.File.LineDelta
	return newNumberToken(i, tmpl)
}

var counter int = 0

func counterMacro(tmpl *Token) *Token {
	counter += 1
	return newNumberToken(counter-1, tmpl)
}

func initMacros() {
	// Define predefined macros
	defineMacro("_LP64", "1")
	defineMacro("__C99_MACRO_WITH_VA_ARGS", "1")
	defineMacro("__ELF__", "1")
	defineMacro("__LP64__", "1")
	defineMacro("__SIZEOF_DOUBLE__", "8")
	defineMacro("__SIZEOF_FLOAT__", "4")
	defineMacro("__SIZEOF_INT__", "4")
	defineMacro("__SIZEOF_LONG_DOUBLE__", "8")
	defineMacro("__SIZEOF_LONG_LONG__", "8")
	defineMacro("__SIZEOF_LONG__", "8")
	defineMacro("__SIZEOF_POINTER__", "8")
	defineMacro("__SIZEOF_PTRDIFF_T__", "8")
	defineMacro("__SIZEOF_SHORT__", "2")
	defineMacro("__SIZEOF_SIZE_T__", "8")
	defineMacro("__SIZE_TYPE__", "unsigned long")
	defineMacro("__STDC_HOSTED__", "1")
	defineMacro("__STDC_NO_COMPLEX__", "1")
	defineMacro("__STDC_VERSION__", "201112L")
	defineMacro("__STDC__", "1")
	defineMacro("__USER_LABEL_PREFIX__", "")
	defineMacro("__alignof__", "_Alignof")
	defineMacro("__amd64", "1")
	defineMacro("__amd64__", "1")
	defineMacro("__zcc__", "1")
	defineMacro("__const__", "const")
	defineMacro("__gnu_linux__", "1")
	defineMacro("__inline__", "inline")
	defineMacro("__linux", "1")
	defineMacro("__linux__", "1")
	defineMacro("__signed__", "signed")
	defineMacro("__typeof__", "typeof")
	defineMacro("__unix", "1")
	defineMacro("__unix__", "1")
	defineMacro("__volatile__", "volatile")
	defineMacro("__x86_64", "1")
	defineMacro("__x86_64__", "1")
	defineMacro("linux", "1")
	defineMacro("unix", "1")
	defineMacro("__STDC_UTF_16__", "1")
	defineMacro("__STDC_UTF_32__", "1")

	addBuiltin("__FILE__", fileMacro)
	addBuiltin("__LINE__", lineMacro)
	addBuiltin("__COUNTER__", counterMacro)
	addBuiltin("__TIMESTAMP__", timestampMacro)
	addBuiltin("__BASE_FILE__", baseFileMacro)

	now := time.Now() // 当前时间，包含本地时区
	defineMacro("__DATE__", formatDate(now))
	defineMacro("__TIME__", formatTime(now))
}

func (tok *Token) tokenizeStringLiteral(basety *CType) *Token {
	var t *Token
	if basety.Size == 2 {
		t = readUTF16StringLiteral(tok.File, tok.Location, tok.Location)
	} else {
		t = readUTF32StringLiteral(tok.File, tok.Location, tok.Location, basety)
	}
	t.Next = tok.Next
	return t
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
func joinAdjacentStringLiterals(tok *Token) {
	// First pass: If regular string literals are adjacent to wide
	// string literals, regular string literals are converted to a wide
	// type before concatenation. In this pass, we do the conversion.
	for tok1 := tok; tok1.Kind != TK_EOF; {
		if tok1.Kind != TK_STR || tok1.Next.Kind != TK_STR {
			tok1 = tok1.Next
			continue
		}

		kind := tok1.getStringKind()
		basety := tok1.Ty.Base

		for t := tok1.Next; t.Kind == TK_STR; t = t.Next {
			k := t.getStringKind()
			if kind == STR_NONE {
				kind = k
				basety = t.Ty.Base
			} else if k != STR_NONE && kind != k {
				errorTok(t, "unsupported non-standard concatenation of string literal")
			}
		}

		if basety.Size > 1 {
			for t := tok1; t.Kind == TK_STR; t = t.Next {
				if t.Ty.Base.Size == 1 {
					t1 := t.tokenizeStringLiteral(basety)
					*t = *t1
				}
			}
		}

		for tok1.Kind == TK_STR {
			tok1 = tok1.Next
		}
	}

	// Second pass: concatenate adjacent string literals.
	for tok1 := tok; tok1 != nil && tok1.Kind != TK_EOF; {
		if tok1.Kind != TK_STR || tok1.Next.Kind != TK_STR {
			tok1 = tok1.Next
			continue
		}

		tok2 := tok1.Next
		for tok2.Kind == TK_STR {
			tok2 = tok2.Next
		}

		length := tok1.Ty.ArrayLength
		for t := tok1.Next; t != tok2; t = t.Next {
			length = length + t.Ty.ArrayLength - 1
		}

		buf := make([]int8, tok1.Ty.Base.Size*length)

		i := 0
		for t := tok1; t != tok2; t = t.Next {
			for j := 0; j < int(t.Ty.Size); j++ {
				buf[i+j] = t.StringLiteral[j]
			}
			i = i + int(t.Ty.Size) - int(t.Ty.Base.Size)
		}

		*tok1 = *(tok1.copy())
		tok1.Ty = arrayOf(tok1.Ty.Base, int64(length))
		tok1.StringLiteral = buf
		tok1.Next = tok2
		tok1 = tok2
	}
}

// Entry point function of the preprocessor.
func preprocess(tok *Token) *Token {
	tok = preprocess2(tok)
	if condIncl != nil {
		errorTok(condIncl.Tok, "unterminated conditional directive")
	}
	convertPpTokens(tok)
	joinAdjacentStringLiterals(tok)

	for t := tok; t != nil; t = t.Next {
		t.LineNo += t.LineDelta
	}

	return tok
}
