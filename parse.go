// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.

package main

import (
	"fmt"
	"math"
)

// Scope for local, global variables or typedefs
// or enum constants
type VarScope struct {
	Next      *VarScope
	Name      string
	Variable  *Obj
	TypeDef   *CType
	EnumType  *CType
	EnumValue int
}

// Scope for struct, union or enum tags
type TagScope struct {
	Next *TagScope
	Name string // Struct tag name
	Ty   *CType
}

// Represents a block scope.
type Scope struct {
	Next *Scope
	// C has two block scopes; one is for variables/typedefs and the other is
	// for struct/union/enum tags.
	Variables *VarScope
	Tags      *TagScope
}

// Variable attributes such as typedef or extern.
type VarAttr struct {
	IsTypeDef bool // Is a typedef
	IsStatic  bool
	IsExtern  bool
	IsInline  bool
	Align     int64
}

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
type Initializer struct {
	Next       *Initializer // Next initializer
	Ty         *CType       // Type of the initializer
	Tok        *Token       // Representative token
	IsFlexible bool

	// If it's not an aggregate type and has an initializer,
	// `expr` has an initialization expression.
	Expr *AstNode

	// If it's an initializer for an aggregate type (e.g. array or struct),
	// `children` has initializers for its children.
	Children []*Initializer // Initializers for children

	// Only one member can be initialized for a union.
	// `mem` is used to clarify which member is initialized.
	Member *Member
}

// For local variable initializer.
type InitDesg struct {
	Next     *InitDesg // Next initializer
	Index    int
	Member   *Member
	Variable *Obj // Variable to initialize
}

var uniqueNameId int = 0

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Likewise, global variables are accumulated to this list.
var globals *Obj

var scope *Scope = &Scope{}

// Points to the function object the parser is currently parsing.
var currentFunction *Obj

// Lists of all goto statements and labels in the curent function.
var gotos *AstNode
var labels *AstNode

// Current "goto" and "continue" jump targets.
var breakLabel string
var continueLabel string

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
var currentSwitch *AstNode

func alignDown(n int64, align int64) int64 {
	return alignTo(n-align+1, align)
}

func enterScope() {
	sc := &Scope{}
	sc.Next = scope
	scope = sc
}

func leaveScope() {
	scope = scope.Next
}

// Find a variable by name.
func findVariable(tok *Token) *VarScope {
	for sc := scope; sc != nil; sc = sc.Next {
		for sc2 := sc.Variables; sc2 != nil; sc2 = sc2.Next {
			if tok.isEqual(sc2.Name) {
				return sc2
			}
		}
	}

	return nil
}

func findTag(tok *Token) *CType {
	for sc := scope; sc != nil; sc = sc.Next {
		for sc2 := sc.Tags; sc2 != nil; sc2 = sc2.Next {
			if tok.isEqual(sc2.Name) {
				return sc2.Ty
			}
		}
	}

	return nil
}

func newCast(expr *AstNode, ty *CType) *AstNode {
	expr.addType()

	node := &AstNode{}
	node.Kind = ND_CAST
	node.Tok = expr.Tok
	node.Lhs = expr
	node.Ty = ty.copy()
	return node
}

func pushScope(name string) *VarScope {
	sc := &VarScope{}
	sc.Name = name
	sc.Next = scope.Variables
	scope.Variables = sc
	return sc
}

func pushTagScope(tok *Token, ty *CType) *TagScope {
	sc := &TagScope{}
	sc.Name = B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
	sc.Ty = ty
	sc.Next = scope.Tags
	scope.Tags = sc
	return sc
}

func newNode(kind AstNodeKind, tok *Token) *AstNode {
	node := &AstNode{}
	node.Kind = kind
	node.Tok = tok
	return node
}

func newBinary(kind AstNodeKind, lhs *AstNode, rhs *AstNode, tok *Token) *AstNode {
	node := newNode(kind, tok)
	node.Lhs = lhs
	node.Rhs = rhs
	return node
}

func newUnary(kind AstNodeKind, expr *AstNode, tok *Token) *AstNode {
	node := newNode(kind, tok)
	node.Lhs = expr
	return node
}

func newNum(value int64, tok *Token) *AstNode {
	node := newNode(ND_NUM, tok)
	node.Value = value
	return node
}

func newLong(value int64, tok *Token) *AstNode {
	node := newNum(value, tok)
	node.Value = value
	node.Ty = TyLong
	return node
}

func newULong(value int64, tok *Token) *AstNode {
	node := newNode(ND_NUM, tok)
	node.Value = value
	node.Ty = TyULong
	return node
}

func newVarNode(variable *Obj, tok *Token) *AstNode {
	node := newNode(ND_VAR, tok)
	node.Variable = variable
	return node
}

func newInitializer(ty *CType, isFlexible bool) *Initializer {
	init := &Initializer{}
	init.Ty = ty

	if ty.Kind == TY_ARRAY {
		if isFlexible && ty.Size < 0 {
			init.IsFlexible = true
			return init
		}
		// init->children = calloc(ty->array_len, sizeof(Initializer *));
		init.Children = make([]*Initializer, ty.ArrayLength)
		for i := int64(0); i < ty.ArrayLength; i++ {
			init.Children[i] = newInitializer(ty.Base, false)
		}

		return init
	}

	if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
		// Count the number of struct members.
		length := 0
		for mem := ty.Members; mem != nil; mem = mem.Next {
			length += 1
		}

		init.Children = make([]*Initializer, length)

		for mem := ty.Members; mem != nil; mem = mem.Next {
			if isFlexible && ty.IsFlexible && mem.Next == nil {
				child := &Initializer{}
				child.Ty = mem.Ty
				child.IsFlexible = true
				init.Children[mem.Index] = child
			} else {
				init.Children[mem.Index] = newInitializer(mem.Ty, false)
			}
		}

		return init
	}

	return init
}

func skipExcessElement(tok *Token) *Token {
	if tok.isEqual("{") {
		tok = skipExcessElement(tok.Next)
		return skip(tok, "}")
	}

	assign(&tok, tok)
	return tok
}

// string-initializer = string-literal
func stringInitializer(rest **Token, tok *Token, init *Initializer) {
	if init.IsFlexible {
		*init = *newInitializer(arrayOf(init.Ty.Base, tok.Ty.ArrayLength), false)
	}

	length := init.Ty.ArrayLength
	if init.Ty.ArrayLength > tok.Ty.ArrayLength {
		length = tok.Ty.ArrayLength
	}

	if init.Ty.Base.Size == 1 {
		str := tok.StringLiteral
		for i := 0; i < int(length); i++ {
			init.Children[i].Expr = newNum(int64(str[i]), tok)
		}
	} else if init.Ty.Base.Size == 2 {
		str := tok.StringLiteral
		for i := range int(length) {
			lo := uint16(uint8(str[2*i]))
			hi := uint16(uint8(str[2*i+1])) << 8
			val := hi | lo
			init.Children[i].Expr = newNum(int64(val), tok)
		}
	} else if init.Ty.Base.Size == 4 {
		str := tok.StringLiteral
		for i := 0; i < int(length); i++ {
			val := (uint32(uint8(str[4*i+3])) << 24) | (uint32(uint8(str[4*i+2])) << 16) | (uint32(uint8(str[4*i+1])) << 8) | uint32(uint8(str[4*i]))
			init.Children[i].Expr = newNum(int64(val), tok)
		}
	} else {
		panic("unreachable")
	}
	*rest = tok.Next
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
func countArrayInitElements(tok *Token, ty *CType) int {
	first := true
	dummy := newInitializer(ty.Base, true)
	i := 0
	max := 0

	for !consumeEnd(&tok, tok) {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		if tok.isEqual("[") {
			i = int(constExpr(&tok, tok.Next))
			if tok.isEqual("...") {
				i = int(constExpr(&tok, tok.Next))
			}
			tok = skip(tok, "]")
			designation(&tok, tok, dummy)
		} else {
			initializer2(&tok, tok, dummy)
		}

		i++
		if max < i {
			max = i
		}
	}

	return max
}

// array-designator = "[" const-expr "]"
//
// C99 added the designated initializer to the language, which allows
// programmers to move the "cursor" of an initializer to any element.
// The syntax looks like this:
//
//	int x[10] = { 1, 2, [5]=3, 4, 5, 6, 7 };
//
// `[5]` moves the cursor to the 5th element, so the 5th element of x
// is set to 3. Initialization then continues forward in order, so
// 6th, 7th, 8th and 9th elements are initialized with 4, 5, 6 and 7,
// respectively. Unspecified elements (in this case, 3rd and 4th
// elements) are initialized with zero.
//
// Nesting is allowed, so the following initializer is valid:
//
//	int x[5][10] = { [5][8]=1, 2, 3 };
//
// It sets x[5][8], x[5][9] and x[6][0] to 1, 2 and 3, respectively.
//
// Use `.fieldname` to move the cursor for a struct initializer. E.g.
//
//	struct { int a, b, c; } x = { .c=5 };
//
// The above initializer sets x.c to 5.
func arrayDesignator(rest **Token, tok *Token, ty *CType) int64 {
	start := tok
	i := constExpr(&tok, tok.Next)
	if i >= ty.ArrayLength {
		errorTok(start, "array designator index exceeds array bounds")
	}
	*rest = skip(tok, "]")
	return i
}

// struct-designator = "." ident
func structDesignator(rest **Token, tok *Token, ty *CType) *Member {
	start := tok
	tok = skip(tok, ".")
	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected a field designator")
	}

	for mem := ty.Members; mem != nil; mem = mem.Next {
		// Anonymous struct member
		if mem.Ty.Kind == TY_STRUCT && mem.Name == nil {
			if getStructMember(mem.Ty, tok) != nil {
				*rest = start
				return mem
			}
			continue
		}

		// Regular struct member
		tokName := B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
		if mem.Name.isEqual(tokName) {
			*rest = tok.Next
			return mem
		}
	}
	errorTok(tok, "struct has no such member")
	panic("unreachable")
}

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
func designation(rest **Token, tok *Token, init *Initializer) {
	if tok.isEqual("[") {
		if init.Ty.Kind != TY_ARRAY {
			errorTok(tok, "array index in non-array initializer")
		}
		i := arrayDesignator(&tok, tok, init.Ty)
		designation(&tok, tok, init.Children[i])
		arrayInitializer2(rest, tok, init, i+1)
		return
	}

	if tok.isEqual(".") && init.Ty.Kind == TY_STRUCT {
		mem := structDesignator(&tok, tok, init.Ty)
		designation(&tok, tok, init.Children[mem.Index])
		init.Expr = nil
		structInitializer2(rest, tok, init, mem.Next)
		return
	}

	if tok.isEqual(".") && init.Ty.Kind == TY_UNION {
		mem := structDesignator(&tok, tok, init.Ty)
		init.Member = mem
		designation(rest, tok, init.Children[mem.Index])
		return
	}

	if tok.isEqual(".") {
		errorTok(tok, "field name not in struct or union initializer")
	}

	if tok.isEqual("=") {
		tok = tok.Next
	}

	initializer2(rest, tok, init)
}

func debugToken(tok *Token) {
	switch tok.Kind {
	case TK_EOF:
		println("eof")
	case TK_IDENT:
		println("ident")
	case TK_KEYWORD:
		println("keyword")
	case TK_NUM:
		println("number")
	case TK_PUNCT:
		println("punct")
	case TK_STR:
		println("string")
	}

	fmt.Printf("%s\r\n", B2S((*tok.File.Contents)[tok.Location:tok.Location+tok.Length]))
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
func arrayInitializer1(rest **Token, tok *Token, init *Initializer) {
	tok = skip(tok, "{")

	if init.IsFlexible {
		length := countArrayInitElements(tok, init.Ty)
		*init = *newInitializer(arrayOf(init.Ty.Base, int64(length)), false)
	}

	first := true

	if init.IsFlexible {
		length := countArrayInitElements(tok, init.Ty)
		*init = *newInitializer(arrayOf(init.Ty.Base, int64(length)), false)
	}

	for i := int64(0); !consumeEnd(rest, tok); i++ {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		if tok.isEqual("[") {
			i = arrayDesignator(&tok, tok, init.Ty)
			designation(&tok, tok, init.Children[i])
			continue
		}

		if i < init.Ty.ArrayLength {
			initializer2(&tok, tok, init.Children[i])
		} else {
			tok = skipExcessElement(tok)
		}
	}
}

// array-initializer2 = initializer ("," initializer)*
func arrayInitializer2(rest **Token, tok *Token, init *Initializer, i int64) {
	if init.IsFlexible {
		length := countArrayInitElements(tok, init.Ty)
		*init = *newInitializer(arrayOf(init.Ty.Base, int64(length)), false)
	}

	for ; i < init.Ty.ArrayLength && !tok.isEnd(); i += 1 {
		start := tok
		if i > 0 {
			tok = skip(tok, ",")
		}

		if tok.isEqual("[") || tok.isEqual(".") {
			*rest = start
			return
		}
		initializer2(&tok, tok, init.Children[i])
	}
	*rest = tok
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
func structInitializer1(rest **Token, tok *Token, init *Initializer) {
	tok = skip(tok, "{")

	mem := init.Ty.Members
	first := true

	for !consumeEnd(rest, tok) {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		if tok.isEqual(".") {
			mem = structDesignator(&tok, tok, init.Ty)
			designation(&tok, tok, init.Children[mem.Index])
			mem = mem.Next
			continue
		}

		if mem != nil {
			initializer2(&tok, tok, init.Children[mem.Index])
			mem = mem.Next
		} else {
			tok = skipExcessElement(tok)
		}
	}
}

// struct-initializer2 = initializer ("," initializer)*
func structInitializer2(rest **Token, tok *Token, init *Initializer, mem *Member) {
	first := true

	for ; mem != nil && !tok.isEnd(); mem = mem.Next {
		start := tok
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		if tok.isEqual("[") || tok.isEqual(".") {
			*rest = start
			return
		}
		initializer2(&tok, tok, init.Children[mem.Index])
	}
	*rest = tok
}

func unionInitializer(rest **Token, tok *Token, init *Initializer) {
	// Unlike structs, union initializers take only one initializer,
	// and that initializes the first union member by default.
	// You can initialize other member using a designated initializer.
	if tok.isEqual("{") && tok.Next.isEqual(".") {
		mem := structDesignator(&tok, tok.Next, init.Ty)
		init.Member = mem
		designation(&tok, tok, init.Children[mem.Index])
		*rest = skip(tok, "}")
		return
	}

	init.Member = init.Ty.Members

	if tok.isEqual("{") {
		initializer2(&tok, tok.Next, init.Children[0])
		consume(&tok, tok, ",")
		*rest = skip(tok, "}")
	} else {
		initializer2(rest, tok, init.Children[0])
	}
}

/*
 * initializer = string-initializer | array-initializer
 * 			   | struct-initializer | union-initializer
 *             | assign
 */
func initializer2(rest **Token, tok *Token, init *Initializer) {
	if init.Ty.Kind == TY_ARRAY && tok.Kind == TK_STR {
		stringInitializer(rest, tok, init)
		return
	}

	if init.Ty.Kind == TY_ARRAY {
		if tok.isEqual("{") {
			arrayInitializer1(rest, tok, init)
		} else {
			arrayInitializer2(rest, tok, init, 0)
		}
		return
	}

	if init.Ty.Kind == TY_STRUCT {
		if tok.isEqual("{") {
			structInitializer1(rest, tok, init)
			return
		}

		// A struct can be initialized with another struct. E.g.
		// `struct T x = y;` where y is a variable of type `struct T`.
		// Handle that case first.
		expr := assign(rest, tok)
		expr.addType()
		if expr.Ty.Kind == TY_STRUCT {
			init.Expr = expr
			return
		}

		structInitializer2(rest, tok, init, init.Ty.Members)
		return
	}

	if init.Ty.Kind == TY_UNION {
		unionInitializer(rest, tok, init)
		return
	}

	if tok.isEqual("{") {
		// An initializer for a scalar variable can be surrounded by
		// braces. E.g. `int x = {3};`. Handle that case.
		initializer2(&tok, tok.Next, init)
		*rest = skip(tok, "}")
		return
	}

	init.Expr = assign(rest, tok)
}

func initializer(rest **Token, tok *Token, ty *CType, newTy **CType) *Initializer {
	init := newInitializer(ty, true)
	initializer2(rest, tok, init)

	if (ty.Kind == TY_STRUCT || ty.Kind == TY_UNION) && ty.IsFlexible {
		ty := ty.copy()

		mem := ty.Members
		for mem.Next != nil {
			mem = mem.Next
		}
		mem.Ty = init.Children[mem.Index].Ty
		ty.Size += mem.Ty.Size

		*newTy = ty
		return init
	}
	*newTy = init.Ty
	return init
}

func initDesgExpr(desg *InitDesg, tok *Token) *AstNode {
	if desg.Variable != nil {
		return newVarNode(desg.Variable, tok)
	}

	if desg.Member != nil {
		node := newUnary(ND_MEMBER, initDesgExpr(desg.Next, tok), tok)
		node.Member = desg.Member
		return node
	}

	lhs := initDesgExpr(desg.Next, tok)
	rhs := newNum(int64(desg.Index), tok)
	return newUnary(ND_DEREF, newAdd(lhs, rhs, tok), tok)
}

func createLocalVarInit(init *Initializer, ty *CType, desg *InitDesg, tok *Token) *AstNode {
	if ty.Kind == TY_ARRAY {
		node := newNode(ND_NULL_EXPR, tok)
		for i := int64(0); i < ty.ArrayLength; i++ {
			desg2 := InitDesg{Next: desg, Index: int(i)}
			rhs := createLocalVarInit(init.Children[i], ty.Base, &desg2, tok)
			node = newBinary(ND_COMMA, node, rhs, tok)
		}
		return node
	}

	if ty.Kind == TY_STRUCT && init.Expr == nil {
		node := newNode(ND_NULL_EXPR, tok)
		for mem := ty.Members; mem != nil; mem = mem.Next {
			desg2 := InitDesg{desg, 0, mem, nil}
			rhs := createLocalVarInit(init.Children[mem.Index], mem.Ty, &desg2, tok)
			node = newBinary(ND_COMMA, node, rhs, tok)
		}
		return node
	}

	if ty.Kind == TY_UNION {
		mem := ty.Members
		if init.Member != nil {
			mem = init.Member
		}
		desg2 := InitDesg{}
		desg2.Next = desg
		desg2.Index = 0
		desg2.Member = mem
		return createLocalVarInit(init.Children[mem.Index], mem.Ty, &desg2, tok)
	}

	if init.Expr == nil {
		return newNode(ND_NULL_EXPR, tok)
	}

	lhs := initDesgExpr(desg, tok)
	return newBinary(ND_ASSIGN, lhs, init.Expr, tok)
}

/*
 * A variable definition with an initializer is a shorthand notation
 * for a variable definition followed by assignments. This function
 * generates assignment expressions for an initializer. For example,
 * `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
 * expressions:
 *
 *   x[0][0] = 6;
 *   x[0][1] = 7;
 *   x[1][0] = 8;
 *   x[1][1] = 9;
 */
func localVarInitializer(rest **Token, tok *Token, variable *Obj) *AstNode {
	init := initializer(rest, tok, variable.Ty, &variable.Ty)
	desg := InitDesg{nil, 0, nil, variable}

	// If a partial initializer list is given, the standard requires
	// that unspecified elements are set to 0. Here, we simply
	// zero-initialize the entire memory region of a variable before
	// initializing it with user-supplied values.
	lhs := newNode(ND_MEMZERO, tok)
	lhs.Variable = variable

	rhs := createLocalVarInit(init, variable.Ty, &desg, tok)
	return newBinary(ND_COMMA, lhs, rhs, tok)
}

func newVar(name string, ty *CType) *Obj {
	variable := &Obj{}
	variable.Name = name
	variable.Ty = ty
	variable.Align = ty.Align
	pushScope(name).Variable = variable
	return variable
}

func newLocalVar(name string, ty *CType) *Obj {
	variable := newVar(name, ty)
	variable.IsLocal = true
	variable.Next = locals
	locals = variable
	return variable
}

func newGlobalVar(name string, ty *CType) *Obj {
	variable := newVar(name, ty)
	variable.Next = globals
	variable.IsStatic = true
	variable.IsDefinition = true
	globals = variable
	return variable
}

func newUniqueName() string {
	n := fmt.Sprintf(".L..%d", uniqueNameId)
	uniqueNameId += 1
	return n
}

func newAnonGlobalVar(ty *CType) *Obj {
	return newGlobalVar(newUniqueName(), ty)
}

func newStringLiteral(lit []int8, ty *CType) *Obj {
	variable := newAnonGlobalVar(ty)
	variable.InitData = lit
	return variable
}

func (tok *Token) getIdent() string {
	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected an identifier")
	}
	return B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])
}

func findTypeDef(tok *Token) *CType {
	if tok.Kind == TK_IDENT {
		sc := findVariable(tok)
		if sc != nil {
			return sc.TypeDef
		}
	}
	return nil
}

func skip(tok *Token, op string) *Token {
	if !tok.isEqual(op) {
		e := fmt.Sprintf("expected '%s'", op)
		errorTok(tok, e)
	}
	return tok.Next
}

func consume(rest **Token, tok *Token, str string) bool {
	if tok.isEqual(str) {
		*rest = tok.Next
		return true
	}
	*rest = tok
	return false
}

// typeof-specifier = "(" (expr | typename) ")"
func typeofSpecifier(rest **Token, tok *Token) *CType {
	tok = skip(tok, "(")

	var ty *CType
	if tok.isTypename() {
		ty = typeName(&tok, tok)
	} else {
		node := expr(&tok, tok)
		node.addType()
		ty = node.Ty
	}
	*rest = skip(tok, ")")
	return ty
}

/*
 * declspec = ("void" | "char" | "short" | "int" | "long" | "_Bool"
 *             | "typedef" | "static" | "extern" | "inline"
 *             | "signed" | "unsigned"
 *             | struct-decl | union-decl | typedef-name
 *             | enum-specifier | typeof-specifier
 *             | "const" | "volatile" | "auto" | "register" | "restrict"
 *             | "__restrict" | "__restrict__" | "_Noreturn")+
 *
 * The order of typenames in a type-specifier doesn't matter. For
 * example, `int long static` means the same as `static long int`.
 * That can also be written as `static long` because you can omit
 * `int` if `long` or `short` are specified. However, something like
 * `char int` is not a valid type specifier. We have to accept only a
 * limited combinations of the typenames.
 *
 * In this function, we count the number of occurrences of each typename
 * while keeping the "current" type object that the typenames up
 * until that point represent. When we reach a non-typename token,
 * we returns the current type object.
 */
func declspec(rest **Token, tok *Token, attr *VarAttr) *CType {
	// We use a single integer as counters for all typenames.
	// For example, bits 0 and 1 represents how many times we saw the
	// keyword "void" so far. With this, we can use a switch statement
	// as you can see below.
	const (
		VOID     = 1 << 0
		BOOL     = 1 << 2
		CHAR     = 1 << 4
		SHORT    = 1 << 6
		INT      = 1 << 8
		LONG     = 1 << 10
		FLOAT    = 1 << 12
		DOUBLE   = 1 << 14
		OTHER    = 1 << 16 // struct or union
		SIGNED   = 1 << 17
		UNSIGNED = 1 << 18
	)

	ty := TyInt
	counter := 0

	for tok.isTypename() {
		// Handle storage class specifiers.
		if tok.isEqual("typedef") || tok.isEqual("static") || tok.isEqual("extern") || tok.isEqual("inline") {
			if attr == nil {
				errorTok(tok, "storage class specifier is not allowed in this context")
			}
			if tok.isEqual("typedef") {
				attr.IsTypeDef = true
			} else if tok.isEqual("static") {
				attr.IsStatic = true
			} else if tok.isEqual("extern") {
				attr.IsExtern = true
			} else {
				attr.IsInline = true
			}

			if attr.IsTypeDef {
				if attr.IsExtern || attr.IsStatic || attr.IsInline {
					errorTok(tok, "typedef may not be used together with static, extern or inline")
				}
			}
			tok = tok.Next
			continue
		}

		// These keywords are recognized but ignored.
		if consume(&tok, tok, "const") || consume(&tok, tok, "volatile") || consume(&tok, tok, "auto") || consume(&tok, tok, "register") || consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") || consume(&tok, tok, "__restrict__") || consume(&tok, tok, "_Noreturn") {
			continue
		}

		if tok.isEqual("_Alignas") {
			if attr == nil {
				errorTok(tok, "_Alignas is not allowed in this context")
			}
			tok = skip(tok.Next, "(")

			if tok.isTypename() {
				attr.Align = typeName(&tok, tok).Align
			} else {
				attr.Align = constExpr(&tok, tok)
			}
			tok = skip(tok, ")")
			continue
		}

		// Handle user-defined types.
		ty2 := findTypeDef(tok)
		if tok.isEqual("struct") || tok.isEqual("union") || tok.isEqual("enum") || tok.isEqual("typeof") || ty2 != nil {
			if counter != 0 {
				break
			}

			if tok.isEqual("struct") {
				ty = structDecl(&tok, tok.Next)
			} else if tok.isEqual("union") {
				ty = unionDecl(&tok, tok.Next)
			} else if tok.isEqual("enum") {
				ty = enumSpecifier(&tok, tok.Next)
			} else if tok.isEqual("typeof") {
				ty = typeofSpecifier(&tok, tok.Next)
			} else {
				ty = ty2
				tok = tok.Next
			}

			counter += OTHER
			continue
		}

		// Handle built-in types.
		if tok.isEqual("void") {
			counter += VOID
		} else if tok.isEqual("_Bool") {
			counter += BOOL
		} else if tok.isEqual("char") {
			counter += CHAR
		} else if tok.isEqual("short") {
			counter += SHORT
		} else if tok.isEqual("int") {
			counter += INT
		} else if tok.isEqual("long") {
			counter += LONG
		} else if tok.isEqual("float") {
			counter += FLOAT
		} else if tok.isEqual("double") {
			counter += DOUBLE
		} else if tok.isEqual("signed") {
			counter |= SIGNED
		} else if tok.isEqual("unsigned") {
			counter |= UNSIGNED
		} else {
			errorTok(tok, "expected a typename")
		}

		switch counter {
		case VOID:
			ty = TyVoid
		case BOOL:
			ty = TyBool
		case CHAR, SIGNED + CHAR:
			ty = TyChar
		case UNSIGNED + CHAR:
			ty = TyUChar
		case SHORT, SHORT + INT, SIGNED + SHORT, SIGNED + SHORT + INT:
			ty = TyShort
		case UNSIGNED + SHORT, UNSIGNED + SHORT + INT:
			ty = TyUShort
		case INT, SIGNED, SIGNED + INT:
			ty = TyInt
		case UNSIGNED, UNSIGNED + INT:
			ty = TyUInt
		case LONG, LONG + INT, LONG + LONG, LONG + LONG + INT, SIGNED + LONG, SIGNED + LONG + INT, SIGNED + LONG + LONG, SIGNED + LONG + LONG + INT:
			ty = TyLong
		case UNSIGNED + LONG, UNSIGNED + LONG + INT, UNSIGNED + LONG + LONG, UNSIGNED + LONG + LONG + INT:
			ty = TyULong
		case FLOAT:
			ty = TyFloat
		case DOUBLE:
			ty = TyDouble
		case LONG + DOUBLE:
			ty = TyDouble
		default:
			errorTok(tok, "invalid type")
		}

		tok = tok.Next
	}

	*rest = tok
	return ty
}

/*
 * enum-specifier = ident? "{" enum-list? "}"
 *                | ident ("{" enum-list? "}")?
 *
 * enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
 */
func enumSpecifier(rest **Token, tok *Token) *CType {
	ty := enumType()

	// Read a struct tag.
	var tag *Token = nil
	if tok.Kind == TK_IDENT {
		tag = tok
		tok = tok.Next
	}

	if tag != nil && !tok.isEqual("{") {
		ty := findTag(tag)
		if ty == nil {
			errorTok(tag, "unknown enum type")
		}
		if ty.Kind != TY_ENUM {
			errorTok(tag, "not an enum tag")
		}

		*rest = tok
		return ty
	}

	tok = skip(tok, "{")

	// Read an enum-list.
	var i int = 0
	var val int64 = 0
	for !consumeEnd(rest, tok) {
		if i > 0 {
			tok = skip(tok, ",")
		}
		i += 1

		name := tok.getIdent()
		tok = tok.Next

		if tok.isEqual("=") {
			val = constExpr(&tok, tok.Next)
		}

		sc := pushScope(name)
		sc.EnumType = ty
		sc.EnumValue = int(val)
		val += 1
	}

	if tag != nil {
		pushTagScope(tag, ty)
	}

	return ty
}

// struct-members = (declspec declarator (","  declarator)* ";")*
func structMembers(rest **Token, tok *Token, ty *CType) {
	head := Member{}
	cur := &head
	idx := 0

	for !tok.isEqual("}") {
		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)
		first := true

		// Anonymous struct member
		if (basety.Kind == TY_STRUCT || basety.Kind == TY_UNION) && consume(&tok, tok, ";") {
			mem := &Member{}
			mem.Ty = basety
			mem.Index = idx
			idx += 1
			if attr.Align != 0 {
				mem.Align = attr.Align
			} else {
				mem.Align = mem.Ty.Align
			}
			cur.Next = mem
			cur = cur.Next
			continue
		}

		// Regular struct members
		for !consume(&tok, tok, ";") {
			if !first {
				tok = skip(tok, ",")
			}
			first = false

			mem := &Member{}
			mem.Ty = declarator(&tok, tok, basety)
			mem.Name = mem.Ty.Name
			mem.Index = idx
			idx += 1
			if attr.Align > 0 {
				mem.Align = attr.Align
			} else {
				mem.Align = mem.Ty.Align
			}

			if consume(&tok, tok, ":") {
				mem.IsBitfield = true
				mem.BitWidth = constExpr(&tok, tok)
			}

			cur.Next = mem
			cur = cur.Next
		}
	}

	// If the last element is an array of incomplete type, it's
	// called a "flexible array member". It should behave as if
	// if were a zero-sized array.
	if cur != &head && cur.Ty.Kind == TY_ARRAY && cur.Ty.ArrayLength < 0 {
		cur.Ty = arrayOf(cur.Ty.Base, 0)
		ty.IsFlexible = true
	}

	*rest = tok.Next
	ty.Members = head.Next
}

// struct-union-decl = ident? ("{" struct-members)?
func structUnionDecl(rest **Token, tok *Token) *CType {
	// Read a struct tag.
	var tag *Token = nil
	if tok.Kind == TK_IDENT {
		tag = tok
		tok = tok.Next
	}

	if tag != nil && !tok.isEqual("{") {
		*rest = tok

		ty := findTag(tag)
		if ty != nil {
			return ty
		}

		ty = structType()
		ty.Size = -1
		pushTagScope(tag, ty)
		return ty
	}

	tok = skip(tok, "{")

	// Construct a struct object.
	ty := structType()
	structMembers(rest, tok, ty)

	if tag != nil {
		// If this is a redefinition, overwrite a previous type.
		// Otherwise, register the struct type.
		for sc := scope.Tags; sc != nil; sc = sc.Next {
			if tag.isEqual(sc.Name) {
				*sc.Ty = *ty
				return sc.Ty
			}
		}
		pushTagScope(tag, ty)
	}
	return ty
}

// struct-decl = struct-union-decl
func structDecl(rest **Token, tok *Token) *CType {
	ty := structUnionDecl(rest, tok)
	ty.Kind = TY_STRUCT

	if ty.Size < 0 {
		return ty
	}

	// Assign offsets within the struct to members.
	bits := int64(0)
	for mem := ty.Members; mem != nil; mem = mem.Next {
		sz := mem.Ty.Size
		if mem.IsBitfield && mem.BitWidth == 0 {
			// Zero-width anonymous bitfield has a special meaning.
			// It affects only alignment.
			bits = alignTo(bits, mem.Ty.Size*8)
		} else if mem.IsBitfield {
			if bits/(sz*8) != (bits+mem.BitWidth-1)/(sz*8) {
				bits = alignTo(bits, sz*8)
			}

			mem.Offset = alignDown(bits/8, sz)
			mem.BitOffset = bits % (sz * 8)
			bits += mem.BitWidth
		} else {
			bits = alignTo(bits, mem.Align*8)
			mem.Offset = bits / 8
			bits += mem.Ty.Size * 8
		}

		if ty.Align < mem.Align {
			ty.Align = mem.Align
		}
	}
	ty.Size = alignTo(bits, ty.Align*8) / 8
	return ty
}

// union-decl = struct-union-decl
func unionDecl(rest **Token, tok *Token) *CType {
	ty := structUnionDecl(rest, tok)
	ty.Kind = TY_UNION

	if ty.Size < 0 {
		return ty
	}

	// If union, we don't have to assign offsets because they
	// are already initialized to zero. We need to compute the
	// alignment and the size though.
	for mem := ty.Members; mem != nil; mem = mem.Next {
		if ty.Align < mem.Ty.Align {
			ty.Align = mem.Align
		}

		if ty.Size < mem.Ty.Size {
			ty.Size = mem.Ty.Size
		}
	}
	ty.Size = alignTo(ty.Size, ty.Align)
	return ty
}

// Find a struct member by name.
func getStructMember(ty *CType, tok *Token) *Member {
	for mem := ty.Members; mem != nil; mem = mem.Next {
		// Anonymous struct member
		if (mem.Ty.Kind == TY_STRUCT || mem.Ty.Kind == TY_UNION) && mem.Name == nil {
			if getStructMember(mem.Ty, tok) != nil {
				return mem
			}
			continue
		}

		// Regular struct member
		if mem.Name.isEqual(B2S((*tok.File.Contents)[tok.Location : tok.Location+tok.Length])) {
			return mem
		}
	}

	return nil
}

// Create a node representing a struct member access, such as foo.bar
// where foo is a struct and bar is a member name.
//
// C has a feature called "anonymous struct" which allows a struct to
// have another unnamed struct as a member like this:
//
//	struct { struct { int a; }; int b; } x;
//
// The members of an anonymous struct belong to the outer struct's
// member namespace. Therefore, in the above example, you can access
// member "a" of the anonymous struct as "x.a".
//
// This function takes care of anonymous structs.
func structRef(node *AstNode, tok *Token) *AstNode {
	node.addType()
	if node.Ty.Kind != TY_STRUCT && node.Ty.Kind != TY_UNION {
		errorTok(node.Tok, "not a struct nor a union")
	}

	ty := node.Ty

	for {
		mem := getStructMember(ty, tok)
		if mem == nil {
			errorTok(tok, "no such member")
		}
		node = newUnary(ND_MEMBER, node, tok)
		node.Member = mem
		if mem.Name != nil {
			break
		}
		ty = mem.Ty
	}
	return node
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
func funcParams(rest **Token, tok *Token, ty *CType) *CType {
	if tok.isEqual("void") && tok.Next.isEqual(")") {
		*rest = tok.Next.Next
		return funcType(ty)
	}

	head := CType{}
	cur := &head
	isVariadic := false

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}

		if tok.isEqual("...") {
			isVariadic = true
			tok = tok.Next
			skip(tok, ")")
			break
		}

		ty2 := declspec(&tok, tok, nil)
		ty2 = declarator(&tok, tok, ty2)

		name := ty2.Name

		if ty2.Kind == TY_ARRAY {
			// "array of T" is converted to "pointer to T" only in the parameter
			// context. For example, *argv[] is converted to **argv by this.
			ty2 = pointerTo(ty2.Base)
			ty2.Name = name
		} else if ty2.Kind == TY_FUNC {
			// Likewise, a function is converted to a pointer to a function
			// only in the parameter context.
			ty2 = pointerTo(ty2)
			ty2.Name = name
		}

		cur.Next = ty2.copy()
		cur = cur.Next
	}

	if cur == &head {
		isVariadic = true
	}

	ty = funcType(ty)
	ty.Params = head.Next
	ty.IsVariadic = isVariadic
	*rest = tok.Next
	return ty
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
func arrayDimensions(rest **Token, tok *Token, ty *CType) *CType {
	for tok.isEqual("static") || tok.isEqual("restrict") {
		tok = tok.Next
	}

	if tok.isEqual("]") {
		ty = typeSuffix(rest, tok.Next, ty)
		return arrayOf(ty, -1)
	}

	sz := constExpr(&tok, tok)
	tok = skip(tok, "]")
	ty = typeSuffix(rest, tok, ty)
	return arrayOf(ty, sz)
}

/*
 * type-suffix = "(" func-params
 *	           | "[" array-dimensions
 *	           | ε
 */
func typeSuffix(rest **Token, tok *Token, ty *CType) *CType {
	if tok.isEqual("(") {
		return funcParams(rest, tok.Next, ty)
	}

	if tok.isEqual("[") {
		return arrayDimensions(rest, tok.Next, ty)
	}

	*rest = tok
	return ty
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
func pointers(rest **Token, tok *Token, ty *CType) *CType {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)

		for tok.isEqual("const") || tok.isEqual("volatile") || tok.isEqual("restrict") || tok.isEqual("__restrict") || tok.isEqual("__restrict__") {
			tok = tok.Next
		}
	}

	*rest = tok
	return ty
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(rest **Token, tok *Token, ty *CType) *CType {
	ty = pointers(&tok, tok, ty)

	if tok.isEqual("(") {
		start := tok
		dummy := CType{}
		declarator(&tok, tok.Next, &dummy)
		tok = skip(tok, ")")
		ty = typeSuffix(rest, tok, ty)
		return declarator(&tok, start.Next, ty)
	}

	var name *Token = nil
	var namePos *Token = nil

	if tok.Kind == TK_IDENT {
		name = tok
		tok = tok.Next
	}

	ty = typeSuffix(rest, tok, ty)
	ty.Name = name
	ty.NamePos = namePos
	return ty
}

// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
func abstractDeclarator(rest **Token, tok *Token, ty *CType) *CType {
	ty = pointers(&tok, tok, ty)

	if tok.isEqual("(") {
		start := tok
		dummy := CType{}
		abstractDeclarator(&tok, tok.Next, &dummy)
		tok = skip(tok, ")")
		ty = typeSuffix(rest, tok, ty)
		return abstractDeclarator(&tok, start.Next, ty)
	}

	return typeSuffix(rest, tok, ty)
}

func (tok *Token) isEnd() bool {
	return tok.isEqual("}") || (tok.isEqual(",") && tok.Next.isEqual("}"))
}

func consumeEnd(rest **Token, tok *Token) bool {
	if tok.isEqual("}") {
		*rest = tok.Next
		return true
	}

	if tok.isEqual(",") && tok.Next.isEqual("}") {
		*rest = tok.Next.Next
		return true
	}

	return false
}

// type-name = declspec abstract-declarator
func typeName(rest **Token, tok *Token) *CType {
	ty := declspec(&tok, tok, nil)
	return abstractDeclarator(rest, tok, ty)
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token, basety *CType, attr *VarAttr) *AstNode {
	head := AstNode{}
	cur := &head
	i := 0

	for !tok.isEqual(";") {
		if i > 0 {
			tok = skip(tok, ",")
		}

		i += 1

		ty := declarator(&tok, tok, basety)
		if ty.Kind == TY_VOID {
			errorTok(tok, "variable declared as void")
		}
		if ty.Name == nil {
			errorTok(ty.NamePos, "variable name omitted")
		}

		if attr != nil && attr.IsStatic {
			// static local variable
			variable := newAnonGlobalVar(ty)
			pushScope(ty.Name.getIdent()).Variable = variable
			if tok.isEqual("=") {
				globalVarInitializer(&tok, tok.Next, variable)
			}
			continue
		}

		variable := newLocalVar(ty.Name.getIdent(), ty)
		if attr != nil && attr.Align > 0 {
			variable.Align = attr.Align
		}

		if tok.isEqual("=") {
			expr := localVarInitializer(&tok, tok.Next, variable)
			cur.Next = newUnary(ND_EXPR_STMT, expr, tok)
			cur = cur.Next
		}

		if variable.Ty.Size < 0 {
			errorTok(tok, "variable has incomplete type")
		}
		if variable.Ty.Kind == TY_VOID {
			errorTok(tok, "variable declared as void")
		}
	}

	node := newNode(ND_BLOCK, tok)
	node.Body = head.Next
	*rest = tok.Next
	return node
}

func readBuf(buf *[]int8, offset int64, sz int64) uint64 {
	if sz == 1 {
		return uint64((*buf)[offset+0])
	}
	if sz == 2 {
		hi := uint16((*buf)[offset+1]) << 8
		lo := uint16((*buf)[offset+0]) << 0
		val := hi | lo
		return uint64(val)
	}
	if sz == 4 {
		v1 := uint32((*buf)[offset+3]) << 24
		v2 := uint32((*buf)[offset+2]) << 16
		v3 := uint32((*buf)[offset+1]) << 8
		v4 := uint32((*buf)[offset+0]) << 0
		val := v1 | v2 | v3 | v4
		return uint64(val)
	}
	if sz == 8 {
		v1 := uint64((*buf)[offset+7]) << 56
		v2 := uint64((*buf)[offset+6]) << 48
		v3 := uint64((*buf)[offset+5]) << 40
		v4 := uint64((*buf)[offset+4]) << 32
		v5 := uint64((*buf)[offset+3]) << 24
		v6 := uint64((*buf)[offset+2]) << 16
		v7 := uint64((*buf)[offset+1]) << 8
		v8 := uint64((*buf)[offset+0]) << 0
		val := v1 | v2 | v3 | v4 | v5 | v6 | v7 | v8
		return val
	}
	panic("unreachable")
}

func writeBuf(buf *[]int8, offset int64, val uint64, sz int64) {
	if sz == 1 {
		(*buf)[offset] = int8(val)
		return
	}

	if sz == 2 {
		(*buf)[offset] = int8(val)
		(*buf)[offset+1] = int8(val >> 8)
		return
	}

	if sz == 4 {
		(*buf)[offset] = int8(val)
		(*buf)[offset+1] = int8(val >> 8)
		(*buf)[offset+2] = int8(val >> 16)
		(*buf)[offset+3] = int8(val >> 24)
		return
	}

	if sz == 8 {
		for i := int64(0); i < 8; i++ {
			(*buf)[offset+i] = int8(val >> (i * 8))
		}
		return
	}

	panic("writeBuf: unsupported size")
}

func writeGlobalVarData(cur *Relocation, init *Initializer, ty *CType, buf *[]int8, offset int64) *Relocation {
	if ty.Kind == TY_ARRAY {
		sz := ty.Base.Size
		for i := int64(0); i < ty.ArrayLength; i++ {
			cur = writeGlobalVarData(cur, init.Children[i], ty.Base, buf, offset+sz*i)
		}
		return cur
	}

	if ty.Kind == TY_STRUCT {
		for mem := ty.Members; mem != nil; mem = mem.Next {
			if mem.IsBitfield {
				expr := init.Children[mem.Index].Expr
				if expr == nil {
					break
				}

				loc := offset + mem.Offset
				oldVal := readBuf(buf, loc, mem.Ty.Size)
				newVal := eval(expr)
				mask := int64((1 << mem.BitWidth) - 1)
				combined := uint64(oldVal) | uint64((newVal&mask)<<mem.BitOffset)
				writeBuf(buf, loc, combined, mem.Ty.Size)
			} else {
				cur = writeGlobalVarData(cur, init.Children[mem.Index], mem.Ty, buf, offset+mem.Offset)
			}
		}
		return cur
	}

	if ty.Kind == TY_UNION {
		if init.Member == nil {
			return cur
		}
		return writeGlobalVarData(cur, init.Children[init.Member.Index], init.Member.Ty, buf, offset)
	}

	if init.Expr == nil {
		return cur
	}

	if ty.Kind == TY_FLOAT {
		val := math.Float32bits(float32(evalDouble(init.Expr)))
		(*buf)[offset+0] = int8(val)
		(*buf)[offset+1] = int8(val >> 8)
		(*buf)[offset+2] = int8(val >> 16)
		(*buf)[offset+3] = int8(val >> 24)
		return cur
	}

	if ty.Kind == TY_DOUBLE {
		val := math.Float64bits(evalDouble(init.Expr))
		(*buf)[offset+0] = int8(val)
		(*buf)[offset+1] = int8(val >> 8)
		(*buf)[offset+2] = int8(val >> 16)
		(*buf)[offset+3] = int8(val >> 24)
		(*buf)[offset+4] = int8(val >> 32)
		(*buf)[offset+5] = int8(val >> 40)
		(*buf)[offset+6] = int8(val >> 48)
		(*buf)[offset+7] = int8(val >> 56)
		return cur
	}

	if ty.Kind == TY_DOUBLE {
		return cur
	}

	label := ""
	val := eval2(init.Expr, &label)

	if label == "" {
		writeBuf(buf, offset, uint64(val), ty.Size)
		return cur
	}

	rel := &Relocation{}
	rel.Offset = offset
	rel.Label = label
	rel.Addend = val
	cur.Next = rel
	return cur.Next
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
func globalVarInitializer(rest **Token, tok *Token, variable *Obj) {
	init := initializer(rest, tok, variable.Ty, &variable.Ty)

	head := Relocation{}
	// char *buf = calloc(1, var->ty->size);
	buf := make([]int8, variable.Ty.Size)
	writeGlobalVarData(&head, init, variable.Ty, &buf, 0)
	variable.InitData = buf
	variable.Rel = head.Next
}

// Returns true if a given token represents a type.
func (tok *Token) isTypename() bool {
	kw := []string{
		"void", "char", "short", "int", "long", "struct", "union", "typedef", "_Bool", "enum", "static", "extern", "_Alignas", "signed", "unsigned", "const", "volatile", "auto", "register", "restrict", "__restrict", "__restrict__", "_Noreturn", "float", "double", "typeof", "inline",
	}

	for _, k := range kw {
		if tok.isEqual(k) {
			return true
		}
	}

	return findTypeDef(tok) != nil
}

/*
 * stmt = "return" expr? ";"
 *	    | "if" "(" expr ")" stmt ("else" stmt)?
 *      | "switch" "(" expr ")" stmt
 *	    | "case" const-expr ":" stmt
 *	    | "default" ":" stmt
 *	    | "for" "(" expr-stmt expr? ";" expr? ")" stmt
 *	    | "while" "(" expr ")" stmt
 *	    | "do" stmt "while" "(" expr ")" ";"
 *      | "asm" asm-stmt
 *      | "goto" ident ";"
 *      | "break" ";"
 *	    | "continue" ";"
 *	    | ident ":" stmt
 *	    | "{" compound-stmt
 *	    | expr-stmt
 */
func stmt(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("return") {
		node := newNode(ND_RETURN, tok)
		if consume(rest, tok.Next, ";") {
			return node
		}
		exp := expr(&tok, tok.Next)
		*rest = skip(tok, ";")

		exp.addType()
		ty := currentFunction.Ty.ReturnType
		if ty.Kind != TY_STRUCT && ty.Kind != TY_UNION {
			exp = newCast(exp, ty)
		}
		node.Lhs = exp
		return node
	}

	if tok.isEqual("if") {
		node := newNode(ND_IF, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")
		node.Then = stmt(&tok, tok)
		if tok.isEqual("else") {
			node.Else = stmt(&tok, tok.Next)
		}
		*rest = tok
		return node
	}

	if tok.isEqual("switch") {
		node := newNode(ND_SWITCH, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")

		sw := currentSwitch
		currentSwitch = node

		brk := breakLabel
		node.BreakLabel = newUniqueName()
		breakLabel = node.BreakLabel

		node.Then = stmt(rest, tok)

		currentSwitch = sw
		breakLabel = brk
		return node
	}

	if tok.isEqual("case") {
		if currentSwitch == nil {
			errorTok(tok, "stray case")
		}

		node := newNode(ND_CASE, tok)
		val := constExpr(&tok, tok.Next)
		tok = skip(tok, ":")
		node.Label = newUniqueName()
		node.Lhs = stmt(rest, tok)
		node.Value = val
		node.CaseNext = currentSwitch.CaseNext
		currentSwitch.CaseNext = node
		return node
	}

	if tok.isEqual("default") {
		if currentSwitch == nil {
			errorTok(tok, "stray default")
		}
		node := newNode(ND_CASE, tok)
		tok = skip(tok.Next, ":")
		node.Label = newUniqueName()
		node.Lhs = stmt(rest, tok)
		currentSwitch.DefaultCase = node
		return node
	}

	if tok.isEqual("for") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")

		enterScope()

		brk := breakLabel
		cont := continueLabel
		node.BreakLabel = newUniqueName()
		breakLabel = node.BreakLabel
		node.ContinueLabel = newUniqueName()
		continueLabel = node.ContinueLabel

		if tok.isTypename() {
			basety := declspec(&tok, tok, nil)
			node.Init = declaration(&tok, tok, basety, nil)
		} else {
			node.Init = exprStmt(&tok, tok)
		}

		if !tok.isEqual(";") {
			node.Cond = expr(&tok, tok)
		}
		tok = skip(tok, ";")

		if !tok.isEqual(")") {
			node.Inc = expr(&tok, tok)
		}
		tok = skip(tok, ")")

		node.Then = stmt(rest, tok)
		leaveScope()
		breakLabel = brk
		continueLabel = cont
		return node
	}

	if tok.isEqual("while") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")

		brk := breakLabel
		cont := continueLabel
		node.BreakLabel = newUniqueName()
		breakLabel = node.BreakLabel
		node.ContinueLabel = newUniqueName()
		continueLabel = node.ContinueLabel

		node.Then = stmt(rest, tok)
		breakLabel = brk
		continueLabel = cont
		return node
	}

	if tok.isEqual("{") {
		return compoundStmt(rest, tok.Next)
	}

	if tok.isEqual("do") {
		node := newNode(ND_DO, tok)

		brk := breakLabel
		cont := continueLabel
		node.BreakLabel = newUniqueName()
		breakLabel = node.BreakLabel
		node.ContinueLabel = newUniqueName()
		continueLabel = node.ContinueLabel

		node.Then = stmt(&tok, tok.Next)

		breakLabel = brk
		continueLabel = cont

		tok = skip(tok, "while")
		tok = skip(tok, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")
		*rest = skip(tok, ";")
		return node
	}

	if tok.isEqual("asm") {
		return asmStmt(rest, tok)
	}

	if tok.isEqual("goto") {
		node := newNode(ND_GOTO, tok)
		node.Label = tok.Next.getIdent()
		node.GotoNext = gotos
		gotos = node
		*rest = skip(tok.Next.Next, ";")
		return node
	}

	if tok.isEqual("break") {
		if breakLabel == "" {
			errorTok(tok, "stray break")
		}
		node := newNode(ND_GOTO, tok)
		node.UniqueLabel = breakLabel
		*rest = skip(tok.Next, ";")
		return node
	}

	if tok.isEqual("continue") {
		if continueLabel == "" {
			errorTok(tok, "stray continue")
		}
		node := newNode(ND_GOTO, tok)
		node.UniqueLabel = continueLabel
		*rest = skip(tok.Next, ";")
		return node
	}

	if tok.Kind == TK_IDENT && tok.Next.isEqual(":") {
		node := newNode(ND_LABEL, tok)
		node.Label = tok.getIdent()
		node.UniqueLabel = newUniqueName()
		node.Lhs = stmt(rest, tok.Next.Next)
		node.GotoNext = labels
		labels = node

		return node
	}

	return exprStmt(rest, tok)
}

/*
 * This function matches gotos with labels.
 *
 * We cannot resolve gotos as we parse a function because gotos
 * can refer a label that appears later in the function.
 * So, we need to do this after we parse the entire function.
 */
func resolveGotoLabels() {
	for x := gotos; x != nil; x = x.GotoNext {
		for y := labels; y != nil; y = y.GotoNext {
			if x.Label == y.Label {
				x.UniqueLabel = y.UniqueLabel
				break
			}
		}

		if x.UniqueLabel == "" {
			errorTok(x.Tok.Next, "use of undeclared label")
		}
	}

	labels = nil
	gotos = nil
}

// compound-stmt = (typedef | declaration | stmt)* "}"
func compoundStmt(rest **Token, tok *Token) *AstNode {
	node := newNode(ND_BLOCK, tok)
	head := AstNode{}
	cur := &head

	enterScope()

	for !tok.isEqual("}") {
		if tok.isTypename() && !tok.Next.isEqual(":") {
			attr := VarAttr{}
			basety := declspec(&tok, tok, &attr)

			if attr.IsTypeDef {
				tok = parseTypeDef(tok, basety)
				continue
			}

			if tok.isFunction() {
				tok = function(tok, basety, &attr)
				continue
			}

			if attr.IsExtern {
				tok = globalVariable(tok, basety, &attr)
				continue
			}

			cur.Next = declaration(&tok, tok, basety, &attr)
			cur = cur.Next
		} else {
			cur.Next = stmt(&tok, tok)
			cur = cur.Next
		}

		cur.addType()
	}

	leaveScope()

	node.Body = head.Next
	*rest = tok.Next

	return node
}

// expr-stmt = expr? ";"
func exprStmt(rest **Token, tok *Token) *AstNode {
	if tok.isEqual(";") {
		*rest = tok.Next
		return newNode(ND_BLOCK, tok)
	}

	node := newNode(ND_EXPR_STMT, tok)
	node.Lhs = expr(&tok, tok)
	*rest = skip(tok, ";")
	return node
}

func evalDouble(node *AstNode) float64 {
	node.addType()

	if node.Ty.isInteger() {
		if node.Ty.IsUnsigned {
			return float64(uint64(eval(node)))
		}
		return float64(eval(node))
	}

	switch node.Kind {
	case ND_ADD:
		return evalDouble(node.Lhs) + evalDouble(node.Rhs)
	case ND_SUB:
		return evalDouble(node.Lhs) - evalDouble(node.Rhs)
	case ND_MUL:
		return evalDouble(node.Lhs) * evalDouble(node.Rhs)
	case ND_DIV:
		return evalDouble(node.Lhs) / evalDouble(node.Rhs)
	case ND_NEG:
		return -evalDouble(node.Lhs)
	case ND_COND:
		if evalDouble(node.Cond) != 0 {
			return evalDouble(node.Then)
		} else {
			return evalDouble(node.Else)
		}
	case ND_COMMA:
		return evalDouble(node.Rhs)
	case ND_CAST:
		if node.Lhs.Ty.isFloat() {
			return evalDouble(node.Lhs)
		}
		return float64(eval(node.Lhs))
	case ND_NUM:
		return node.FloatValue
	}

	errorTok(node.Tok, "not a compile-time constant")
	panic("unreachable")
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
func eval2(node *AstNode, label *string) int64 {
	node.addType()

	if node.Ty.isFloat() {
		return int64(evalDouble(node))
	}

	switch node.Kind {
	case ND_ADD:
		return eval2(node.Lhs, label) + eval(node.Rhs)
	case ND_SUB:
		return eval2(node.Lhs, label) - eval(node.Rhs)
	case ND_MUL:
		return eval(node.Lhs) * eval(node.Rhs)
	case ND_DIV:
		if node.Ty.IsUnsigned {
			return int64(uint64(eval(node.Lhs)) / uint64(eval(node.Rhs)))
		}
		return eval(node.Lhs) / eval(node.Rhs)
	case ND_NEG:
		return -eval(node.Lhs)
	case ND_MOD:
		if node.Ty.IsUnsigned {
			return int64(uint64(eval(node.Lhs) % eval(node.Rhs)))
		}
		return eval(node.Lhs) % eval(node.Rhs)
	case ND_BITAND:
		return eval(node.Lhs) & eval(node.Rhs)
	case ND_BITOR:
		return eval(node.Lhs) | eval(node.Rhs)
	case ND_BITXOR:
		return eval(node.Lhs) ^ eval(node.Rhs)
	case ND_SHL:
		return eval(node.Lhs) << eval(node.Rhs)
	case ND_SHR:
		if node.Ty.IsUnsigned && node.Ty.Size == 8 {
			return int64(uint64(eval(node.Lhs) >> eval(node.Rhs)))
		}
		return eval(node.Lhs) >> eval(node.Rhs)
	case ND_EQ:
		if eval(node.Lhs) == eval(node.Rhs) {
			return 1
		}
		return 0
	case ND_NE:
		if eval(node.Lhs) != eval(node.Rhs) {
			return 1
		}
		return 0
	case ND_LT:
		if node.Lhs.Ty.IsUnsigned {
			if eval(node.Rhs) < 0 {
				return 0
			} else {
				if uint64(eval(node.Lhs)) < uint64(eval(node.Rhs)) {
					return 1
				} else {
					return 0
				}
			}
		}
		if eval(node.Lhs) < eval(node.Rhs) {
			return 1
		}
		return 0
	case ND_LE:
		if node.Lhs.Ty.IsUnsigned {
			if eval(node.Rhs) < 0 {
				return 0
			} else {
				if uint64(eval(node.Lhs)) <= uint64(eval(node.Rhs)) {
					return 1
				} else {
					return 0
				}
			}
		}
		if eval(node.Lhs) <= eval(node.Rhs) {
			return 1
		}
		return 0
	case ND_COND:
		if eval(node.Cond) != 0 {
			return eval2(node.Then, label)
		} else {
			return eval2(node.Else, label)
		}
	case ND_COMMA:
		return eval2(node.Rhs, label)
	case ND_NOT:
		if eval(node.Lhs) == 0 {
			return 1
		}
		return 0
	case ND_BITNOT:
		return ^eval(node.Lhs)
	case ND_LOGAND:
		if eval(node.Lhs) != 0 && eval(node.Rhs) != 0 {
			return 1
		}
		return 0
	case ND_LOGOR:
		if eval(node.Lhs) != 0 || eval(node.Rhs) != 0 {
			return 1
		}
		return 0
	case ND_CAST:
		val := eval2(node.Lhs, label)
		if node.Ty.isInteger() {
			switch node.Ty.Size {
			case 1:
				if node.Ty.IsUnsigned {
					return int64(uint8(val))
				}
				return int64(int8(val))
			case 2:
				if node.Ty.IsUnsigned {
					return int64(uint16(val))
				}
				return int64(int16(val))
			case 4:
				if node.Ty.IsUnsigned {
					return int64(uint32(val))
				}
				return int64(int32(val))
			}
		}
		return val
	case ND_ADDR:
		return evalRval(node.Lhs, label)
	case ND_MEMBER:
		if label == nil {
			errorTok(node.Tok, "not a compile-time constant")
		}
		if node.Ty.Kind != TY_ARRAY {
			errorTok(node.Tok, "invalid initializer")
		}
		return evalRval(node.Lhs, label) + node.Member.Offset
	case ND_VAR:
		if label == nil {
			errorTok(node.Tok, "not a compile-time constant")
		}
		if node.Variable.Ty.Kind != TY_ARRAY && node.Variable.Ty.Kind != TY_FUNC {
			errorTok(node.Tok, "invalid initializer")
		}
		*label = node.Variable.Name
		return 0
	case ND_NUM:
		return node.Value
	}

	errorTok(node.Tok, "not a compile-time constant")
	panic("unreachable")
}

func eval(node *AstNode) int64 {
	return eval2(node, nil)
}

func evalRval(node *AstNode, label *string) int64 {
	switch node.Kind {
	case ND_VAR:
		if node.Variable.IsLocal {
			errorTok(node.Tok, "not a compile-time constant")
		}
		*label = node.Variable.Name
		return 0
	case ND_DEREF:
		return eval2(node.Lhs, label)
	case ND_MEMBER:
		return evalRval(node.Lhs, label) + node.Member.Offset
	}

	errorTok(node.Tok, "invalid initializer")
	panic("unreachable")
}

func constExpr(rest **Token, tok *Token) int64 {
	node := conditional(rest, tok)
	return eval(node)
}

// Convert op= operators to expressions containing an assignment.
//
// In general, `A op= C` is converted to “tmp = &A, *tmp = *tmp op B`.
// However, if a given expression is of form `A.x op= C`, the input is
// converted to `tmp = &A, (*tmp).x = (*tmp).x op C` to handle assignments
// to bitfields.
func toAssign(binary *AstNode) *AstNode {
	binary.Lhs.addType()
	binary.Rhs.addType()

	tok := binary.Tok

	// Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
	if binary.Lhs.Kind == ND_MEMBER {
		v := newLocalVar("", pointerTo(binary.Lhs.Lhs.Ty))

		expr1 := newBinary(ND_ASSIGN, newVarNode(v, tok), newUnary(ND_ADDR, binary.Lhs.Lhs, tok), tok)
		expr2 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(v, tok), tok), tok)
		expr2.Member = binary.Lhs.Member
		expr3 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(v, tok), tok), tok)
		expr3.Member = binary.Lhs.Member
		expr4 := newBinary(ND_ASSIGN, expr2, newBinary(binary.Kind, expr3, binary.Rhs, tok), tok)
		return newBinary(ND_COMMA, expr1, expr4, tok)
	}

	// Convert `A op= C` to ``tmp = &A, *tmp = *tmp op B`.
	variable := newLocalVar("", pointerTo(binary.Lhs.Ty))

	expr1 := newBinary(ND_ASSIGN, newVarNode(variable, tok), newUnary(ND_ADDR, binary.Lhs, tok), tok)

	expr2 := newBinary(ND_ASSIGN, newUnary(ND_DEREF, newVarNode(variable, tok), tok), newBinary(binary.Kind, newUnary(ND_DEREF, newVarNode(variable, tok), tok), binary.Rhs, tok), tok)

	return newBinary(ND_COMMA, expr1, expr2, tok)
}

// expr = assign ("," expr)?
func expr(rest **Token, tok *Token) *AstNode {
	node := assign(&tok, tok)
	if tok.isEqual(",") {
		return newBinary(ND_COMMA, node, expr(rest, tok.Next), tok)
	}

	*rest = tok
	return node
}

// asm-stmt = "asm" ("volatile" | "inline")* "(" string-literal ")"
func asmStmt(rest **Token, tok *Token) *AstNode {
	node := newNode(ND_ASM, tok)
	tok = tok.Next

	for tok.isEqual("volatile") || tok.isEqual("inline") {
		tok = tok.Next
	}

	tok = skip(tok, "(")
	if tok.Kind != TK_STR || tok.Ty.Base.Kind != TY_CHAR {
		errorTok(tok, "expected string literal")
	}

	node.AsmStr = B2S(tok.StringLiteral)
	*rest = skip(tok.Next, ")")

	return node
}

// conditional = logor ("?" expr? ":" conditional)?
func conditional(rest **Token, tok *Token) *AstNode {
	cond := logor(&tok, tok)

	if !tok.isEqual("?") {
		*rest = tok
		return cond
	}

	if tok.Next.isEqual(":") {
		// [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
		cond.addType()
		v := newLocalVar("", cond.Ty)
		lhs := newBinary(ND_ASSIGN, newVarNode(v, tok), cond, tok)
		rhs := newNode(ND_COND, tok)
		rhs.Cond = newVarNode(v, tok)
		rhs.Then = newVarNode(v, tok)
		rhs.Else = conditional(rest, tok.Next.Next)
		return newBinary(ND_COMMA, lhs, rhs, tok)
	}

	node := newNode(ND_COND, tok)
	node.Cond = cond
	node.Then = expr(&tok, tok.Next)
	tok = skip(tok, ":")
	node.Else = conditional(rest, tok)
	return node
}

/*
 * assign = conditional (assign-op assign)?
 * assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
 *           | "<<= | ">>="
 */
func assign(rest **Token, tok *Token) *AstNode {
	node := conditional(&tok, tok)

	if tok.isEqual("=") {
		return newBinary(ND_ASSIGN, node, assign(rest, tok.Next), tok)
	}

	if tok.isEqual("+=") {
		return toAssign(newAdd(node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("-=") {
		return toAssign(newSub(node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("*=") {
		return toAssign(newBinary(ND_MUL, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("/=") {
		return toAssign(newBinary(ND_DIV, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("%=") {
		return toAssign(newBinary(ND_MOD, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("&=") {
		return toAssign(newBinary(ND_BITAND, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("|=") {
		return toAssign(newBinary(ND_BITOR, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("^=") {
		return toAssign(newBinary(ND_BITXOR, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual("<<=") {
		return toAssign(newBinary(ND_SHL, node, assign(rest, tok.Next), tok))
	}

	if tok.isEqual(">>=") {
		return toAssign(newBinary(ND_SHR, node, assign(rest, tok.Next), tok))
	}

	*rest = tok
	return node
}

// logor = logand ("||" logand)*
func logor(rest **Token, tok *Token) *AstNode {
	node := logand(&tok, tok)
	for tok.isEqual("||") {
		start := tok
		node = newBinary(ND_LOGOR, node, logand(&tok, tok.Next), start)
	}
	*rest = tok
	return node
}

// logand = bitor ("&&" bitor)*
func logand(rest **Token, tok *Token) *AstNode {
	node := bitor(&tok, tok)
	for tok.isEqual("&&") {
		start := tok
		node = newBinary(ND_LOGAND, node, bitor(&tok, tok.Next), start)
	}
	*rest = tok
	return node
}

// bitor = bitxor ("|" bitxor)*
func bitor(rest **Token, tok *Token) *AstNode {
	node := bitxor(&tok, tok)
	for tok.isEqual("|") {
		start := tok
		node = newBinary(ND_BITOR, node, bitxor(&tok, tok.Next), start)
	}
	*rest = tok
	return node
}

// bitxor = bitand ("^" bitand)*
func bitxor(rest **Token, tok *Token) *AstNode {
	node := bitand(&tok, tok)
	for tok.isEqual("^") {
		start := tok
		node = newBinary(ND_BITXOR, node, bitand(&tok, tok.Next), start)
	}
	*rest = tok
	return node
}

// bitand = equality ("&" equality)*
func bitand(rest **Token, tok *Token) *AstNode {
	node := equality(&tok, tok)
	for tok.isEqual("&") {
		start := tok
		node = newBinary(ND_BITAND, node, equality(&tok, tok.Next), start)
	}
	*rest = tok
	return node
}

// equality = relational ("==" relational | "!=" relational)*
func equality(rest **Token, tok *Token) *AstNode {
	node := relational(&tok, tok)

	for {
		start := tok

		if tok.isEqual("==") {
			node = newBinary(ND_EQ, node, relational(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("!=") {
			node = newBinary(ND_NE, node, relational(&tok, tok.Next), start)
			continue
		}

		*rest = tok
		return node
	}
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
func relational(rest **Token, tok *Token) *AstNode {
	node := shift(&tok, tok)

	for {
		start := tok

		if tok.isEqual("<") {
			node = newBinary(ND_LT, node, shift(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("<=") {
			node = newBinary(ND_LE, node, shift(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual(">") {
			node = newBinary(ND_LT, shift(&tok, tok.Next), node, start)
			continue
		}

		if tok.isEqual(">=") {
			node = newBinary(ND_LE, shift(&tok, tok.Next), node, start)
			continue
		}

		*rest = tok
		return node
	}
}

// shift = add ("<<" add | ">>" add)*
func shift(rest **Token, tok *Token) *AstNode {
	node := add(&tok, tok)

	for {
		start := tok

		if tok.isEqual("<<") {
			node = newBinary(ND_SHL, node, add(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual(">>") {
			node = newBinary(ND_SHR, node, add(&tok, tok.Next), start)
			continue
		}

		*rest = tok
		return node
	}
}

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
func newAdd(lhs *AstNode, rhs *AstNode, tok *Token) *AstNode {
	lhs.addType()
	rhs.addType()

	// num + num
	if lhs.Ty.isNumeric() && rhs.Ty.isNumeric() {
		return newBinary(ND_ADD, lhs, rhs, tok)
	}

	if lhs.Ty.Base != nil && rhs.Ty.Base != nil {
		errorTok(tok, "invalid operands")
	}

	// Canonicalize `num + ptr` to `ptr + num`.
	if lhs.Ty.Base == nil && rhs.Ty.Base != nil {
		tmp := lhs
		lhs = rhs
		rhs = tmp
	}

	// ptr + num
	rhs = newBinary(ND_MUL, rhs, newLong(lhs.Ty.Base.Size, tok), tok)
	return newBinary(ND_ADD, lhs, rhs, tok)
}

// Like `+`, `-` is overloaded for the pointer type.
func newSub(lhs *AstNode, rhs *AstNode, tok *Token) *AstNode {
	lhs.addType()
	rhs.addType()

	// num - num
	if lhs.Ty.isNumeric() && rhs.Ty.isNumeric() {
		return newBinary(ND_SUB, lhs, rhs, tok)
	}

	// ptr - num
	if lhs.Ty.Base != nil && rhs.Ty.isInteger() {
		rhs = newBinary(ND_MUL, rhs, newLong(lhs.Ty.Base.Size, tok), tok)
		rhs.addType()
		node := newBinary(ND_SUB, lhs, rhs, tok)
		node.Ty = lhs.Ty
		return node
	}

	// ptr - ptr, which returns how many elements are between the two.
	if lhs.Ty.Base != nil && rhs.Ty.Base != nil {
		node := newBinary(ND_SUB, lhs, rhs, tok)
		node.Ty = TyLong
		return newBinary(ND_DIV, node, newNum(lhs.Ty.Base.Size, tok), tok)
	}

	errorTok(tok, "invalid operands")
	return nil
}

// add = mul ("+" mul | "-" mul)*
func add(rest **Token, tok *Token) *AstNode {
	node := mul(&tok, tok)

	for {
		start := tok

		if tok.isEqual("+") {
			rhs := mul(&tok, tok.Next)
			node = newAdd(node, rhs, start)
			continue
		}

		if tok.isEqual("-") {
			node = newSub(node, mul(&tok, tok.Next), start)
			continue
		}

		*rest = tok
		return node
	}
}

// mul = unary ("*" cast | "/" cast | "%" cast)*
func mul(rest **Token, tok *Token) *AstNode {
	node := castExpr(&tok, tok)

	for {
		start := tok

		if tok.isEqual("*") {
			node = newBinary(ND_MUL, node, castExpr(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("/") {
			node = newBinary(ND_DIV, node, castExpr(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("%") {
			node = newBinary(ND_MOD, node, castExpr(&tok, tok.Next), start)
			continue
		}

		*rest = tok
		return node
	}
}

// cast = "(" type-name ")" cast | unary
func castExpr(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("(") && tok.Next.isTypename() {
		start := tok
		ty := typeName(&tok, tok.Next)
		tok = skip(tok, ")")

		// compound literal
		if tok.isEqual("{") {
			return unary(rest, start)
		}

		// type cast
		node := newCast(castExpr(rest, tok), ty)
		node.Tok = start
		return node
	}

	return unary(rest, tok)
}

/*
 * unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
 *       | ("++" | "--") unary
 *       | postfix
 */
func unary(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("+") {
		return castExpr(rest, tok.Next)
	}

	if tok.isEqual("-") {
		return newUnary(ND_NEG, castExpr(rest, tok.Next), tok)
	}

	if tok.isEqual("&") {
		lhs := castExpr(rest, tok.Next)
		lhs.addType()
		if lhs.Kind == ND_MEMBER && lhs.Member.IsBitfield {
			errorTok(tok, "cannot take address of bitfield")
		}
		return newUnary(ND_ADDR, lhs, tok)
	}

	if tok.isEqual("*") {
		// [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
		// in the C spec, but dereferencing a function shouldn't do
		// anything. If foo is a function, `*foo`, `**foo` or `*****foo`
		// are all equivalent to just `foo`.
		node := castExpr(rest, tok.Next)
		node.addType()
		if node.Ty.Kind == TY_FUNC {
			return node
		}

		return newUnary(ND_DEREF, node, tok)
	}

	if tok.isEqual("!") {
		return newUnary(ND_NOT, castExpr(rest, tok.Next), tok)
	}

	if tok.isEqual("~") {
		return newUnary(ND_BITNOT, castExpr(rest, tok.Next), tok)
	}

	// Read ++i as i+=1
	if tok.isEqual("++") {
		return toAssign(newAdd(unary(rest, tok.Next), newNum(1, tok), tok))
	}

	// Read --i as i-=1
	if tok.isEqual("--") {
		return toAssign(newSub(unary(rest, tok.Next), newNum(1, tok), tok))
	}

	return postfix(rest, tok)
}

// Convert A++ to `(typeof A)((A += 1) - 1)`
func newIncDec(node *AstNode, tok *Token, addend int) *AstNode {
	node.addType()
	return newCast(newAdd(toAssign(newAdd(node, newNum(int64(addend), tok), tok)), newNum(-int64(addend), tok), tok), node.Ty)
}

/*
 * postfix = "(" type-name ")" "{" initializer-list "}"
 *         = ident "(" func-args ")" postfix-tail*
 *         | primary postfix-tail*
 *
 * postfix-tail = "[" expr "]"
 *              | "(" func-args ")"
 *              | "." ident
 *              | "->" ident
 *              | "++"
 *              | "--"
 */
func postfix(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("(") && tok.Next.isTypename() {
		// Compound literal
		start := tok
		ty := typeName(&tok, tok.Next)
		tok = skip(tok, ")")

		if scope.Next == nil {
			variable := newAnonGlobalVar(ty)
			globalVarInitializer(rest, tok, variable)
			return newVarNode(variable, start)
		}

		variable := newLocalVar("", ty)
		lhs := localVarInitializer(rest, tok, variable)
		rhs := newVarNode(variable, tok)
		return newBinary(ND_COMMA, lhs, rhs, start)
	}

	node := primary(&tok, tok)

	for {
		if tok.isEqual("(") {
			node = funcall(&tok, tok.Next, node)
			continue
		}

		if tok.isEqual(("[")) {
			// x[y] is short for *(x+y)
			start := tok
			idx := expr(&tok, tok.Next)
			tok = skip(tok, "]")
			node = newUnary(ND_DEREF, newAdd(node, idx, start), start)
			continue
		}

		if tok.isEqual(".") {
			node = structRef(node, tok.Next)
			tok = tok.Next.Next
			continue
		}

		if tok.isEqual("->") {
			// x->y is short for (*x).y
			node = structRef(newUnary(ND_DEREF, node, tok), tok.Next)
			tok = tok.Next.Next
			continue
		}

		if tok.isEqual("++") {
			node = newIncDec(node, tok, 1)
			tok = tok.Next
			continue
		}

		if tok.isEqual("--") {
			node = newIncDec(node, tok, -1)
			tok = tok.Next
			continue
		}

		*rest = tok
		return node
	}
}

// funcall = (assign ("," assign)*)? ")"
func funcall(rest **Token, tok *Token, fn *AstNode) *AstNode {
	fn.addType()

	if fn.Ty.Kind != TY_FUNC && (fn.Ty.Kind != TY_PTR || fn.Ty.Base.Kind != TY_FUNC) {
		errorTok(fn.Tok, "not a function")
	}

	var ty *CType
	if fn.Ty.Kind == TY_FUNC {
		ty = fn.Ty
	} else {
		ty = fn.Ty.Base
	}
	paramTy := ty.Params

	head := AstNode{}
	cur := &head

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}

		arg := assign(&tok, tok)
		arg.addType()

		if paramTy == nil && !ty.IsVariadic {
			errorTok(arg.Tok, "too many arguments to function")
		}

		if paramTy != nil {
			if paramTy.Kind != TY_STRUCT && paramTy.Kind != TY_UNION {
				arg = newCast(arg, paramTy)
			}
			paramTy = paramTy.Next
		} else if arg.Ty.Kind == TY_FLOAT {
			// If parameter type is omitted (e.g. in "..."), float
			// arguments are promoted to double.
			arg = newCast(arg, TyDouble)
		}

		cur.Next = arg
		cur = cur.Next
	}

	if paramTy != nil {
		errorTok(tok, "not enough arguments to function")
	}

	*rest = skip(tok, ")")

	node := newUnary(ND_FUNCALL, fn, tok)
	node.FuncType = ty
	node.Ty = ty.ReturnType
	node.Args = head.Next

	// If a function returns a struct, it is caller's responsibility
	// to allocate a space for the return value.
	if node.Ty.Kind == TY_STRUCT || node.Ty.Kind == TY_UNION {
		node.ReturnBuffer = newLocalVar("", node.Ty)
	}
	return node
}

/*
	generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"

//
// generic-assoc = type-name ":" assign
//               | "default" ":" assign
*/
func genericSelection(rest **Token, tok *Token) *AstNode {
	start := tok
	tok = skip(tok, "(")

	ctrl := assign(&tok, tok)
	ctrl.addType()

	t1 := ctrl.Ty
	if t1.Kind == TY_FUNC {
		t1 = pointerTo(t1)
	} else if t1.Kind == TY_ARRAY {
		t1 = pointerTo(t1.Base)
	}

	var ret *AstNode = nil

	for !consume(rest, tok, ")") {
		tok = skip(tok, ",")

		if tok.isEqual("default") {
			tok = skip(tok.Next, ":")
			node := assign(&tok, tok)
			if ret == nil {
				ret = node
			}
			continue
		}

		t2 := typeName(&tok, tok)
		tok = skip(tok, ":")
		node := assign(&tok, tok)
		if t1.isCompatibleWith(t2) {
			ret = node
		}
	}

	if ret == nil {
		errorTok(start, "controlling expression type not compatible with any generic association type")
	}

	return ret
}

/*
 * primary = "(" "{" stmt+ "}" ")"
 *         | "(" expr ")"
 *         | "sizeof" "(" type-name ")"
 *         | "sizeof" unary
 *         | "_Alignof" "(" type-name ")"
 *         | "_Alignof" unary
 *         | "_Generic" generic-selection
 *         | "__builtin_types_compatible_p" "(" type-name, type-name, ")"
 *         | "__builtin_reg_class" "(" type-name ")"
 *         | ident
 *         | str
 *         | num
 */
func primary(rest **Token, tok *Token) *AstNode {
	start := tok

	if tok.isEqual("(") && tok.Next.isEqual("{") {
		// This is a GNU statement expresssion.
		node := newNode(ND_STMT_EXPR, tok)
		node.Body = compoundStmt(&tok, tok.Next.Next).Body
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("sizeof") && tok.Next.isEqual("(") && tok.Next.Next.isTypename() {
		ty := typeName(&tok, tok.Next.Next)
		*rest = skip(tok, ")")
		return newULong(ty.Size, start)
	}

	if tok.isEqual("(") {
		node := expr(&tok, tok.Next)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("sizeof") {
		node := unary(rest, tok.Next)
		node.addType()
		return newULong(node.Ty.Size, tok)
	}

	if tok.isEqual("_Alignof") && tok.Next.isEqual("(") && tok.Next.Next.isTypename() {
		ty := typeName(&tok, tok.Next.Next)
		*rest = skip(tok, ")")
		return newULong(ty.Align, tok)
	}

	if tok.isEqual("_Alignof") {
		node := unary(rest, tok.Next)
		node.addType()
		return newULong(node.Ty.Align, tok)
	}

	if tok.isEqual("_Generic") {
		return genericSelection(rest, tok.Next)
	}

	if tok.isEqual("__builtin_types_compatible_p") {
		tok = skip(tok.Next, "(")
		t1 := typeName(&tok, tok)
		tok = skip(tok, ",")
		t2 := typeName(&tok, tok)
		*rest = skip(tok, ")")
		if t1.isCompatibleWith(t2) {
			return newNum(1, start)
		}
		return newNum(0, start)
	}

	if tok.isEqual("__builtin_reg_class") {
		tok = skip(tok.Next, "(")
		ty := typeName(&tok, tok)
		*rest = skip(tok, ")")

		if ty.isInteger() || ty.Kind == TY_PTR {
			return newNum(0, start)
		}
		if ty.isFloat() {
			return newNum(1, start)
		}

		return newNum(2, start)
	}

	if tok.Kind == TK_IDENT {
		// Variable or enum constant
		sc := findVariable(tok)
		*rest = tok.Next

		if sc != nil {
			if sc.Variable != nil {
				return newVarNode(sc.Variable, tok)
			} else {
				return newNum(int64(sc.EnumValue), tok)
			}
		}

		if tok.Next.isEqual("(") {
			errorTok(tok, "implicit declaration of a function")
		}
		errorTok(tok, "undefined variable")
	}

	if tok.Kind == TK_STR {
		v := newStringLiteral(tok.StringLiteral, tok.Ty)
		*rest = tok.Next
		return newVarNode(v, tok)
	}

	if tok.Kind == TK_NUM {
		var node *AstNode = nil
		if tok.Ty.isFloat() {
			node = newNode(ND_NUM, tok)
			node.FloatValue = tok.FloatValue
		} else {
			node = newNum(tok.Value, tok)
		}

		node.Ty = tok.Ty
		*rest = tok.Next
		return node
	}
	errorTok(tok, "expected an expression")
	return nil
}

func parseTypeDef(tok *Token, basety *CType) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		if ty.Name == nil {
			errorTok(ty.NamePos, "typedef name omitted")
		}
		pushScope(ty.Name.getIdent()).TypeDef = ty
	}

	return tok
}

func createParamLocalVars(param *CType) {
	if param != nil {
		createParamLocalVars(param.Next)
		if param.Name == nil {
			errorTok(param.NamePos, "parameter name omitted")
		}
		newLocalVar(param.Name.getIdent(), param)
	}
}

func function(tok *Token, basety *CType, attr *VarAttr) *Token {
	ty := declarator(&tok, tok, basety)
	if ty.Name == nil {
		errorTok(ty.NamePos, "function name omitted")
	}

	fn := newGlobalVar(ty.Name.getIdent(), ty)
	fn.IsFunction = true
	fn.IsDefinition = !consume(&tok, tok, ";")
	fn.IsStatic = attr.IsStatic || (attr.IsInline && !attr.IsExtern)
	fn.IsInline = attr.IsInline

	if !fn.IsDefinition {
		return tok
	}

	currentFunction = fn
	locals = nil

	enterScope()
	createParamLocalVars(ty.Params)

	// A buffer for a struct/union return value is passed
	// as the hidden first parameter.
	rty := ty.ReturnType
	if (rty.Kind == TY_STRUCT || rty.Kind == TY_UNION) && rty.Size > 16 {
		newLocalVar("", pointerTo(rty))
	}

	fn.Params = locals
	if ty.IsVariadic {
		fn.VaArea = newLocalVar("__va_area__", arrayOf(TyChar, 136))
	}

	tok = skip(tok, "{")

	// [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
	// automatically defined as a local variable containing the
	// current function name.
	buf := []uint8(fn.Name)
	buf = append(buf, 0)
	newBuf := U82I8(buf)
	pushScope("__func__").Variable = newStringLiteral(newBuf, arrayOf(TyChar, int64(len(fn.Name)+1)))

	// [GNU] __FUNCTION__ is yet another name of __func__.
	pushScope("__FUNCTION__").Variable = newStringLiteral(newBuf, arrayOf(TyChar, int64(len(fn.Name)+1)))

	fn.Body = compoundStmt(&tok, tok)
	fn.Locals = locals
	leaveScope()
	resolveGotoLabels()
	return tok
}

func globalVariable(tok *Token, basety *CType, attr *VarAttr) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		if ty.Name == nil {
			errorTok(ty.NamePos, "variable name omitted")
		}

		variable := newGlobalVar(ty.Name.getIdent(), ty)
		variable.IsDefinition = !attr.IsExtern
		variable.IsStatic = attr.IsStatic
		if attr.Align > 0 {
			variable.Align = attr.Align
		}
		if tok.isEqual("=") {
			globalVarInitializer(&tok, tok.Next, variable)
		}
	}

	return tok
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
func (tok *Token) isFunction() bool {
	if tok.isEqual(";") {
		return false
	}

	dummy := CType{}
	ty := declarator(&tok, tok, &dummy)
	return ty.Kind == TY_FUNC
}

// program = (function-definition | global-variable)*
func parse(tok *Token) *Obj {
	globals = nil

	for tok.Kind != TK_EOF {
		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)

		// Typedef
		if attr.IsTypeDef {
			tok = parseTypeDef(tok, basety)
			continue
		}

		// Function
		if tok.isFunction() {
			tok = function(tok, basety, &attr)
			continue
		}

		// Global variable
		tok = globalVariable(tok, basety, &attr)
	}

	return globals
}
