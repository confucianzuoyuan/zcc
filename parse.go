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
	"math/big"
	"os"
	"strings"
)

var TypeNames = map[string]struct{}{
	"void":          {},
	"char":          {},
	"short":         {},
	"int":           {},
	"long":          {},
	"struct":        {},
	"union":         {},
	"typedef":       {},
	"_Bool":         {},
	"enum":          {},
	"static":        {},
	"extern":        {},
	"_Alignas":      {},
	"signed":        {},
	"unsigned":      {},
	"const":         {},
	"volatile":      {},
	"auto":          {},
	"register":      {},
	"restrict":      {},
	"__restrict":    {},
	"__restrict__":  {},
	"_Noreturn":     {},
	"float":         {},
	"double":        {},
	"inline":        {},
	"_Thread_local": {},
	"__thread":      {},
	"_Atomic":       {},
	"__typeof":      {},
	"__typeof__":    {},
	"__auto_type":   {},
}

// Scope for local, global variables or typedefs
// or enum constants
type VarScope struct {
	Variable  *Obj
	TypeDef   *CType
	EnumType  *CType
	EnumValue int
}

// Variable attributes such as typedef or extern.
type VarAttr struct {
	IsTypeDef   bool // Is a typedef
	IsStatic    bool
	IsExtern    bool
	IsInline    bool
	IsTls       bool
	IsConstExpr bool
	Align       int64
}

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
type Initializer struct {
	Ty         *CType // Type of the initializer
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

type EvalKind int

const (
	EV_CONST EvalKind = iota // constant expression
	EV_LABEL                 // relocation label
	EV_AGG                   // "constexpr" aggregate
)

type EvalContext struct {
	Kind  EvalKind
	Var   *Obj
	Label *string
}

var CurrentVLA *Obj
var BreakVLA *Obj
var FnUseVLA bool
var DontDeallocVLA bool

var evalRecover *bool = nil

var uniqueNameId int = 0

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

func evalError(tok *Token, msg string) int64 {
	if evalRecover != nil {
		*evalRecover = true
		return 0
	}

	verrorAt(tok.File.Name, tok.File.Contents, tok.LineNo, tok.Location, msg)
	os.Exit(1)
	panic("unreachable")
}

func alignDown(n int64, align int64) int64 {
	return alignTo(n-align+1, align)
}

func readDoubleBuf(buf *[]int8, ty *CType) FloatConst {
	if ty.Kind == TY_FLOAT {
		return FloatConst32{Int8SliceToFloat32((*buf)[0:4])}
	}
	if ty.Kind == TY_DOUBLE {
		return FloatConst64{Int8SliceToFloat64((*buf)[0:8])}
	}
	if ty.Kind == TY_LDOUBLE {
		var bs []byte = I82U8((*buf)[0:10])
		var arr [10]byte
		copy(arr[:], bs) // 将 slice 复制到 arr 中
		return FloatConst80{longDoubleToBigFloat(arr)}
	}
	panic("unreachable")
}

func enterScope() {
	sc := &Scope{}
	sc.Parent = scope
	sc.SiblingNext = scope.Children
	scope.Children = sc
	scope = scope.Children
}

func leaveScope() {
	scope = scope.Parent
}

func enterTmpScope() {
	enterScope()
	scope.IsTemporary = true
}

// Find a variable by name.
func findVariable(tok *Token) *VarScope {
	for sc := scope; sc != nil; sc = sc.Parent {
		name := tok.getText()
		sc2 := sc.Vars[name]
		if sc2 != nil {
			return sc2
		}
	}

	return nil
}

func findTag(tok *Token) *CType {
	for sc := scope; sc != nil; sc = sc.Parent {
		name := tok.getText()
		ty := sc.Tags[name]
		if ty != nil {
			return ty
		}
	}

	return nil
}

func (expr *AstNode) toBool() *AstNode {
	return newCast(expr, TyBool)
}

func commaList(rest **Token, tokRest **Token, end string, skipComma bool) bool {
	tok := *tokRest
	if consume(rest, tok, end) {
		return false
	}

	if skipComma {
		tok = skip(tok, ",")

		// curly brackets allow trailing comma
		if end == "}" && consume(rest, tok, "}") {
			return false
		}

		*tokRest = tok
	}

	return true
}

// Generate code for computing a VLA size.
func computeVlaSize(ty *CType, tok *Token) *AstNode {
	if ty.VlaSize != nil {
		return nil
	}

	var node *AstNode = nil
	if ty.Base != nil {
		node = computeVlaSize(ty.Base, tok)
	}

	if ty.Kind != TY_VLA {
		return node
	}

	var baseSize *AstNode
	if ty.Base.Kind == TY_VLA {
		baseSize = newVarNode(ty.Base.VlaSize, tok)
	} else {
		baseSize = newNum(ty.Base.Size, tok)
	}

	ty.VlaSize = newLocalVar("", TyULong)
	expr := newBinary(ND_ASSIGN, newVarNode(ty.VlaSize, tok), newBinary(ND_MUL, ty.VlaLen, baseSize, tok), tok)
	chainExpr(&node, expr)
	node.addType()
	return node
}

func chainExpr(lhs **AstNode, rhs *AstNode) {
	if rhs != nil {
		if *lhs == nil {
			*lhs = rhs
		} else {
			*lhs = newBinary(ND_CHAIN, *lhs, rhs, rhs.Tok)
		}
	}
}

func loopBody(rest **Token, tok *Token, node *AstNode) {
	brk := breakLabel
	cont := continueLabel
	node.BreakLabel = newUniqueName()
	node.ContinueLabel = newUniqueName()
	breakLabel = node.BreakLabel
	continueLabel = node.ContinueLabel

	vla := BreakVLA
	BreakVLA = CurrentVLA

	node.Then = stmt(rest, tok, true)

	breakLabel = brk
	continueLabel = cont
	BreakVLA = vla
}

func newVLA(sz *AstNode, v *Obj, align int64) *AstNode {
	node := newUnary(ND_ALLOCA, sz, sz.Tok)
	node.Ty = pointerTo(TyVoid)
	node.Variable = v
	node.Value = align
	sz.addType()
	return node
}

func (node *AstNode) isConstDouble(fval *FloatConst) bool {
	node.addType()
	failed := false

	if evalRecover != nil {
		panic("evalRecover != nil")
	}
	evalRecover = &failed
	v := evalDouble(node)
	// println(node.Kind.String(), ": ", "cast from ", node.Lhs.Ty.String(), " to ", node.Ty.String(), "; ", v.String())
	if fval != nil {
		*fval = v
	}
	evalRecover = nil
	return !failed
}

func (node *AstNode) isConstExpr(val *int64) bool {
	node.addType()
	failed := false

	if evalRecover != nil {
		panic("evalRecover != nil")
	}
	evalRecover = &failed
	v := eval(node)
	if val != nil {
		*val = v
	}
	evalRecover = nil
	return !failed
}

func newCast(expr *AstNode, ty *CType) *AstNode {
	expr.addType()

	tmpNode := AstNode{
		Kind: ND_CAST,
		Tok:  expr.Tok,
		Lhs:  expr,
		Ty:   ty,
	}
	if opt_optimize {
		if (ty.isInteger() && tmpNode.isConstExpr(&tmpNode.Value)) || (ty.isFloat() && tmpNode.isConstDouble(&tmpNode.FloatValue)) {
			// s := ""
			// if expr.FloatValue != nil {
			// 	s += expr.FloatValue.String()
			// } else {
			// 	s += fmt.Sprintf("%d", expr.Value)
			// }
			// s += " to "
			// if tmpNode.FloatValue != nil {
			// 	s += tmpNode.FloatValue.String()
			// } else {
			// 	s += fmt.Sprintf("%d", tmpNode.Value)
			// }
			// println("cast from `" + expr.Ty.String() + "` to `" + ty.String() + "`; " + s)
			expr.Kind = ND_NUM
			expr.Value = tmpNode.Value
			expr.FloatValue = tmpNode.FloatValue
			expr.Ty = ty
			return expr
		}
		if expr.Ty == ty && !expr.isBitField() {
			return expr
		}
	}
	node := &AstNode{}
	*node = tmpNode
	return node
}

func evalVoid(node *AstNode) {
	if node.Ty.isFloat() {
		evalDouble(node)
		return
	}
	eval(node)
}

func pushScope(name string) *VarScope {
	sc := &VarScope{}
	if scope.Vars == nil {
		scope.Vars = make(map[string]*VarScope)
	}
	scope.Vars[name] = sc
	return sc
}

func pushTagScope(tok *Token, ty *CType) {
	name := tok.getText()
	if scope.Tags == nil {
		scope.Tags = make(map[string]*CType)
	}
	scope.Tags[name] = ty
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
			mem.Index = length
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
func stringInitializer(tok *Token, init *Initializer) {
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
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
func countArrayInitElements(tok *Token, ty *CType) int {
	dummy := newInitializer(ty.Base, true)
	i := 0
	max := 0

	for commaList(&tok, &tok, "}", i != 0) {
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
func arrayDesignator(rest **Token, tok *Token, ty *CType, begin *int64, end *int64) {
	*begin = constExpr(&tok, tok.Next)
	if *begin >= ty.ArrayLength {
		errorTok(tok, "array designator index exceeds array bounds")
	}

	if tok.isEqual("...") {
		*end = constExpr(&tok, tok.Next)
		if *end >= ty.ArrayLength {
			errorTok(tok, "array designator index exceeds array bounds")
		}
		if *end < *begin {
			errorTok(tok, fmt.Sprintf("array designator range [%d, %d] is empty", *begin, *end))
		}
	} else {
		*end = *begin
	}
	*rest = skip(tok, "]")
}

// struct-designator = "." ident
func structDesignator(rest **Token, tok *Token, ty *CType) *Member {
	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected a field designator")
	}

	mem := getStructMember(ty, tok)
	if mem == nil {
		errorTok(tok, "struct has no such member")
	}
	if mem.Name != nil {
		*rest = tok.Next
	}
	return mem
}

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
func designation(rest **Token, tok *Token, init *Initializer) {
	if tok.isEqual("[") {
		if init.Ty.Kind != TY_ARRAY {
			errorTok(tok, "array index in non-array initializer")
		}

		begin := int64(0)
		end := int64(0)
		arrayDesignator(&tok, tok, init.Ty, &begin, &end)

		var tok2 *Token
		for i := begin; i <= end; i++ {
			designation(&tok2, tok, init.Children[i])
		}
		arrayInitializer2(rest, tok2, init, begin+1)
		return
	}

	if tok.isEqual(".") && init.Ty.Kind == TY_STRUCT {
		mem := structDesignator(&tok, tok.Next, init.Ty)
		designation(&tok, tok, init.Children[mem.Index])
		init.Expr = nil
		structInitializer2(rest, tok, init, mem.Next, true)
		return
	}

	if tok.isEqual(".") && init.Ty.Kind == TY_UNION {
		mem := structDesignator(&tok, tok.Next, init.Ty)
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

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
func arrayInitializer1(rest **Token, tok *Token, init *Initializer) {
	tok = skip(tok, "{")

	if init.IsFlexible {
		length := countArrayInitElements(tok, init.Ty)
		*init = *newInitializer(arrayOf(init.Ty.Base, int64(length)), false)
	}

	first := true

	for i := int64(0); commaList(rest, &tok, "}", !first); i++ {
		if tok.isEqual("[") {
			begin := int64(0)
			end := int64(0)
			arrayDesignator(&tok, tok, init.Ty, &begin, &end)

			var tok2 *Token
			for j := begin; j <= end; j++ {
				designation(&tok2, tok, init.Children[j])
			}
			tok = tok2
			i = end
			first = false
			continue
		}

		if i < init.Ty.ArrayLength {
			initializer2(&tok, tok, init.Children[i])
		} else {
			tok = skipExcessElement(tok)
		}

		first = false
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

	for ; commaList(rest, &tok, "}", !first); first = false {
		if tok.isEqual(".") {
			mem = structDesignator(&tok, tok.Next, init.Ty)
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
func structInitializer2(rest **Token, tok *Token, init *Initializer, mem *Member, postDesig bool) {
	first := true

	for ; mem != nil && !tok.isEnd(); mem = mem.Next {
		start := tok
		if !first || postDesig {
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
	tok = skip(tok, "{")
	first := true

	for ; commaList(rest, &tok, "}", !first); first = false {
		if tok.isEqual(".") {
			init.Member = structDesignator(&tok, tok.Next, init.Ty)
			designation(&tok, tok, init.Children[init.Member.Index])
			continue
		}

		if first && init.Ty.Members != nil {
			init.Member = init.Ty.Members
			initializer2(&tok, tok, init.Children[0])
		} else {
			tok = skipExcessElement(tok)
		}
	}
}

/*
 * initializer = string-initializer | array-initializer
 * 			   | struct-initializer | union-initializer
 *             | assign
 */
func initializer2(rest **Token, tok *Token, init *Initializer) {
	if init.Ty.Kind == TY_ARRAY && init.Ty.Base.isInteger() {
		start := tok
		strToken := &Token{}
		if tok.isEqual("{") && isStringToken(&tok, tok.Next, &strToken) {
			if consume(rest, tok, "}") {
				stringInitializer(strToken, init)
				return
			}
			tok = start
		}
		if isStringToken(rest, tok, &strToken) {
			stringInitializer(strToken, init)
			return
		}
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

		if init.Ty.Members == nil {
			errorTok(tok, "initializer for empty aggregate requires explicit braces")
		}

		structInitializer2(rest, tok, init, init.Ty.Members, false)
		return
	}

	if init.Ty.Kind == TY_UNION {
		if tok.isEqual("{") {
			unionInitializer(rest, tok, init)
			return
		}

		expr := assign(rest, tok)
		expr.addType()
		if expr.Ty.Kind == TY_UNION {
			init.Expr = expr
			return
		}
		if init.Ty.Members == nil {
			errorTok(tok, "initializer for empty aggregate requires explicit braces")
		}

		init.Member = init.Ty.Members
		initializer2(rest, tok, init.Children[0])
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

	if ty.Kind == TY_STRUCT && ty.IsFlexible {
		ty = ty.copy()

		head := Member{}
		cur := &head
		for mem := ty.Members; mem != nil; mem = mem.Next {
			m := &Member{}
			*m = *mem
			cur.Next = m
			cur = cur.Next
		}

		cur.Ty = init.Children[cur.Index].Ty
		ty.Size += cur.Ty.Size
		ty.Members = head.Next

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
		if init.Expr != nil {
			panic("init.Expr != nil")
		}
		var node *AstNode = nil
		for i := int64(0); i < ty.ArrayLength; i++ {
			desg2 := InitDesg{Next: desg, Index: int(i)}
			chainExpr(&node, createLocalVarInit(init.Children[i], ty.Base, &desg2, tok))
		}
		return node
	}

	if init.Expr != nil {
		lhs := initDesgExpr(desg, tok)
		return newBinary(ND_ASSIGN, lhs, init.Expr, tok)
	}

	if ty.Kind == TY_STRUCT {
		var node *AstNode = nil
		for mem := ty.Members; mem != nil; mem = mem.Next {
			desg2 := InitDesg{desg, 0, mem, nil}
			chainExpr(&node, createLocalVarInit(init.Children[mem.Index], mem.Ty, &desg2, tok))
		}
		return node
	}

	if ty.Kind == TY_UNION {
		if init.Member == nil {
			return nil
		}
		desg2 := InitDesg{
			Next:   desg,
			Index:  0,
			Member: init.Member,
		}
		return createLocalVarInit(init.Children[init.Member.Index], init.Member.Ty, &desg2, tok)
	}

	return nil
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

	expr := createLocalVarInit(init, variable.Ty, &desg, tok)
	if opt_optimize && init.Expr != nil {
		return expr
	}

	// If a partial initializer list is given, the standard requires
	// that unspecified elements are set to 0. Here, we simply
	// zero-initialize the entire memory region of a variable before
	// initializing it with user-supplied values.
	node := newNode(ND_MEMZERO, tok)
	node.Variable = variable
	chainExpr(&node, expr)
	return node
}

func newVar(name string, ty *CType) *Obj {
	variable := &Obj{}
	variable.Name = name
	variable.Ty = ty
	variable.Align = ty.Align
	if name != "" {
		pushScope(name).Variable = variable
	}
	return variable
}

func newLocalVar(name string, ty *CType) *Obj {
	variable := newVar(name, ty)
	variable.IsLocal = true
	variable.Next = scope.Locals
	scope.Locals = variable
	return variable
}

func newGlobalVar(name string, ty *CType) *Obj {
	variable := newVar(name, ty)
	variable.Next = globals
	globals = variable
	return variable
}

func newUniqueName() string {
	n := fmt.Sprintf(".L..%d", uniqueNameId)
	uniqueNameId += 1
	return n
}

func newAnonymousGlobalVariable(ty *CType) *Obj {
	v := newGlobalVar("", ty)
	v.Name = newUniqueName()
	v.IsDefinition = true
	v.IsStatic = true
	return v
}

func newStringLiteral(lit []int8, ty *CType) *Obj {
	variable := newAnonymousGlobalVariable(ty)
	variable.InitData = lit
	return variable
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

func pointerQualifiers(rest **Token, tok *Token, ty *CType) {
	for ; ; tok = tok.Next {
		if tok.isEqual("_Atomic") {
			ty.IsAtomic = true
		} else if tok.isEqual("const") {
			ty.IsConst = true
		} else if tok.isEqual("volatile") {
			ty.IsVolatile = true
		} else if tok.isEqual("restrict") || tok.isEqual("__restrict") || tok.isEqual("__restrict__") {
			ty.IsRestrict = true
		} else {
			break
		}
	}
	*rest = tok
}

func funcParamsOldStyle(rest **Token, tok *Token, funcType *CType) *CType {
	start := tok
	tok = skipParen(tok)

	enterScope()
	funcType.Scopes = scope
	var expr *AstNode = nil

	for tok.isTypename() {
		basety := declspec(&tok, tok, nil)
		for {
			var name *Token = nil
			ty := declarator(&tok, tok, basety, &name)
			if name == nil {
				errorTok(tok, "expected identifier")
			}

			var promoted *Obj = nil
			if ty.isInteger() && ty.Size < TyInt.Size {
				promoted = newLocalVar("", TyInt)
			} else if ty.Kind == TY_FLOAT {
				promoted = newLocalVar("", TyDouble)
			} else if ty.isArray() {
				ty = pointerTo(ty.Base)
			} else if ty.Kind == TY_FUNC {
				ty = pointerTo(ty)
			}

			v := newLocalVar(name.getIdent(), ty)
			if promoted != nil {
				v.ParamPromoted = promoted
				chainExpr(&expr, newBinary(ND_ASSIGN, newVarNode(v, tok), newVarNode(promoted, tok), tok))
			}
			chainExpr(&expr, computeVlaSize(ty, tok))
			if !commaList(&tok, &tok, ";", true) {
				break
			}
		}
	}
	*rest = tok

	head := Obj{}
	cur := &head

	for tok := start; commaList(&tok, &tok, ")", cur != &head); {
		sc, ok := funcType.Scopes.Vars[tok.getIdent()]

		var nxt *Obj
		if !ok {
			nxt = newLocalVar(tok.getIdent(), TyInt)
		} else if sc.Variable.ParamPromoted != nil {
			nxt = sc.Variable.ParamPromoted
		} else {
			nxt = sc.Variable
		}

		cur.ParamNext = nxt
		cur = cur.ParamNext
		tok = tok.Next
	}

	leaveScope()
	expr.addType()
	funcType.ParamList = head.ParamNext
	funcType.IsOldStyle = true
	funcType.PreCalc = expr
	return funcType
}

/*
 * declspec = ("void" | "char" | "short" | "int" | "long" | "_Bool"
 *             | "typedef" | "static" | "extern" | "inline"
 *             | "_Thread_local" | "__thread"
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

	var ty *CType = nil
	counter := 0
	isAtomic := false
	isAuto := false
	isConst := false
	isRestrict := false
	isVolatile := false
	for {
		if attr != nil {
			attrAligned(tok, &attr.Align)
		}
		if !tok.isTypename() {
			break
		}

		// Handle storage class specifiers.
		if tok.isEqual("typedef") || tok.isEqual("static") || tok.isEqual("extern") || tok.isEqual("inline") || tok.isEqual("_Thread_local") || tok.isEqual("__thread") || tok.isEqual("constexpr") {
			if attr == nil {
				errorTok(tok, "storage class specifier is not allowed in this context")
			}
			if tok.isEqual("typedef") {
				attr.IsTypeDef = true
			} else if tok.isEqual("static") {
				attr.IsStatic = true
			} else if tok.isEqual("extern") {
				attr.IsExtern = true
			} else if tok.isEqual("inline") {
				attr.IsInline = true
			} else if tok.isEqual("constexpr") {
				attr.IsConstExpr = true
			} else {
				attr.IsTls = true
			}

			if attr.IsTypeDef {
				if attr.IsExtern || attr.IsStatic || attr.IsInline || attr.IsTls {
					errorTok(tok, "typedef may not be used together with static, extern, inline, __thread or _Thread_local")
				}
			}
			tok = tok.Next
			continue
		}

		// These keywords are recognized but ignored.
		if consume(&tok, tok, "register") || consume(&tok, tok, "_Noreturn") {
			continue
		}

		if tok.isEqual("__auto_type") || tok.isEqual("auto") {
			if tok.isEqual("__auto_type") || opt_std >= STD_C23 {
				isAuto = true
			}
			tok = tok.Next
			continue
		}

		if consume(&tok, tok, "_Atomic") {
			if consume(&tok, tok, "(") {
				ty = typeName(&tok, tok)
				tok = skip(tok, ")")
			}
			isAtomic = true
			continue
		}

		if consume(&tok, tok, "const") {
			isConst = true
			continue
		}

		if consume(&tok, tok, "volatile") {
			isVolatile = true
			continue
		}

		if consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") ||
			consume(&tok, tok, "__restrict__") {
			isRestrict = true
			continue
		}

		if tok.isEqual("_Alignas") {
			if attr == nil {
				errorTok(tok, "_Alignas is not allowed in this context")
			}
			tok = skip(tok.Next, "(")

			var align int64
			if tok.isTypename() {
				align = typeName(&tok, tok).Align
			} else {
				align = constExpr(&tok, tok)
			}
			attr.Align = int64(math.Max(float64(attr.Align), float64(align)))
			tok = skip(tok, ")")
			continue
		}

		ty2 := findTypeDef(tok)
		if ty2 != nil {
			if counter != 0 || isAuto {
				break
			}
			ty = ty2
			tok = tok.Next
			counter += OTHER
			continue
		}
		if tok.isEqual("struct") || tok.isEqual("union") || tok.isEqual("enum") || tok.isEqual("typeof") || tok.isEqual("__typeof") || tok.isEqual("__typeof__") || tok.isEqual("typeof_unqual") {
			if counter != 0 {
				errorTok(tok, "invalid type")
			}

			if tok.isEqual("struct") {
				ty = structUnionDecl(&tok, tok.Next, TY_STRUCT)
			} else if tok.isEqual("union") {
				ty = structUnionDecl(&tok, tok.Next, TY_UNION)
			} else if tok.isEqual("enum") {
				ty = enumSpecifier(&tok, tok.Next)
			} else if tok.isEqual("typeof_unqual") {
				ty = typeofSpecifier(&tok, tok.Next).unqual()
			} else {
				ty = typeofSpecifier(&tok, tok.Next)
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
		case CHAR:
			ty = TyPChar
		case SIGNED + CHAR:
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
		case LONG, LONG + INT, SIGNED + LONG, SIGNED + LONG + INT:
			ty = TyLong
		case LONG + LONG, LONG + LONG + INT, SIGNED + LONG + LONG, SIGNED + LONG + LONG + INT:
			ty = TyLLong
		case UNSIGNED + LONG, UNSIGNED + LONG + INT:
			ty = TyULong
		case UNSIGNED + LONG + LONG, UNSIGNED + LONG + LONG + INT:
			ty = TyULLong
		case FLOAT:
			ty = TyFloat
		case DOUBLE:
			ty = TyDouble
		case LONG + DOUBLE:
			ty = TyLDouble
		default:
			errorTok(tok, "invalid type")
		}

		tok = tok.Next
	}

	*rest = tok

	if ty == nil && isAuto {
		if tok.Kind != TK_IDENT || !tok.Next.isEqual("=") {
			errorTok(tok, "unsupported form for type inference")
		}
		enterScope()
		dummy := &Token{}
		node := assign(&dummy, tok.Next.Next)
		node.addType()
		leaveScope()
		ty = node.Ty.arrayToPointer().unqual()
	}

	if ty == nil {
		ty = TyInt
	}

	if isAtomic || isConst || isVolatile || isRestrict {
		ty2 := newQualifiedType(ty)
		ty2.IsAtomic = isAtomic
		ty2.IsConst = isConst
		ty2.IsVolatile = isVolatile
		ty2.IsRestrict = isRestrict
		return ty2
	}

	return ty
}

func newQualifiedType(ty *CType) *CType {
	if ty.Origin != nil {
		ty = ty.Origin
	}

	ret := &CType{}
	*ret = *ty
	ret.Origin = ty

	if ty.Size < 0 {
		ret.DeclNext = ty.DeclNext
		ty.DeclNext = ret
	}
	return ret
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
	var val int64 = 0
	first := true
	for ; commaList(rest, &tok, "}", !first); first = false {
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

	for !tok.isEqual("}") {
		if tok.isEqual("_Static_assert") {
			staticAssertion(&tok, tok.Next)
			continue
		}

		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)

		// Anonymous struct member
		if (basety.Kind == TY_STRUCT || basety.Kind == TY_UNION) && tok.isEqual(";") {
			mem := &Member{}
			mem.Ty = basety
			// mem->alt_align = attr.align // clang behavior
			attrAligned(tok, &mem.AltAlign)
			tok = tok.Next
			cur.Next = mem
			cur = cur.Next
			continue
		}

		// Regular struct members
		first := true
		for ; commaList(&tok, &tok, ";", !first); first = false {
			mem := &Member{}
			mem.AltAlign = attr.Align
			mem.Ty = declarator2(&tok, tok, basety, &mem.Name, &mem.AltAlign)

			for t := mem.Ty; t != nil; t = t.Base {
				if t.Kind == TY_VLA {
					errorTok(tok, "members cannot be of variable-modified type")
				}
			}

			if consume(&tok, tok, ":") {
				mem.IsBitfield = true
				mem.BitWidth = constExpr(&tok, tok)
				attrAligned(tok, &mem.AltAlign)
			}

			cur.Next = mem
			cur = cur.Next
		}
	}

	// If the last element is an array of incomplete type, it's
	// called a "flexible array member". It should behave as if
	// if were a zero-sized array.
	if cur != &head && cur.Ty.Kind == TY_ARRAY && cur.Ty.ArrayLength < 0 {
		if ty.Kind != TY_STRUCT {
			errorTok(tok, "flexible array member not allowed in union")
		}
		cur.Ty = arrayOf(cur.Ty.Base, 0)
		ty.IsFlexible = true
	}

	*rest = tok.Next
	ty.Members = head.Next
}

// struct-union-decl = attribute? ident? ("{" struct-members)?
func structUnionDecl(rest **Token, tok *Token, kind CTypeKind) *CType {
	ty := newType(kind, -1, 1)
	altAlign := int64(0)
	attrAligned(tok, &altAlign)
	attrPacked(tok, ty, true)

	// Read a struct tag.
	var tag *Token = nil
	if tok.Kind == TK_IDENT {
		tag = tok
		tok = tok.Next
	}

	if tag != nil && !tok.isEqual("{") {
		*rest = tok

		ty2 := findTag(tag)
		if ty2 != nil {
			return ty2
		}

		pushTagScope(tag, ty)
		return ty
	}

	tok = skip(tok, "{")

	// Construct a struct object.
	structMembers(&tok, tok, ty)

	attrAligned(tok, &altAlign)
	attrPacked(tok, ty, false)
	*rest = tok

	if kind == TY_STRUCT {
		ty = structDecl(ty, altAlign)
	} else {
		ty = unionDecl(ty, altAlign)
	}

	if tag == nil {
		return ty
	}

	name := tag.getText()
	ty2, ok := scope.Tags[name]
	if ok {
		for t := ty2; t != nil; t = t.DeclNext {
			t.Size = ty.Size
			t.Align = int64(math.Max(float64(t.Align), float64(ty.Align)))
			t.Members = ty.Members
			t.IsFlexible = ty.IsFlexible
			t.IsPacked = ty.IsPacked
			t.Origin = ty
		}
		return ty2
	}

	pushTagScope(tag, ty)
	return ty
}

// struct-decl = struct-union-decl
func structDecl(ty *CType, altAlign int64) *CType {
	bits := int64(0)
	head := Member{}
	cur := &head

	for mem := ty.Members; mem != nil; mem = mem.Next {
		affectAlignment := false

		if !mem.IsBitfield || mem.Name != nil {
			cur.Next = mem
			cur = cur.Next
			affectAlignment = true
		}

		if !ty.IsPacked {
			if mem.AltAlign != 0 && (mem.IsBitfield || mem.AltAlign > mem.Ty.Align) {
				bits = alignTo(bits, mem.AltAlign*8)
			}
			if affectAlignment {
				altAlign = int64(math.Max(float64(altAlign), math.Max(float64(mem.AltAlign), float64(mem.Ty.Align))))
			}
		} else if mem.AltAlign != 0 && mem.Ty.Kind != TY_STRUCT && mem.Ty.Kind != TY_UNION {
			bits = alignTo(bits, mem.AltAlign*8)
			if affectAlignment {
				altAlign = int64(math.Max(float64(altAlign), float64(mem.AltAlign)))
			}
		}

		if mem.IsBitfield {
			if mem.BitWidth == 0 {
				bits = alignTo(bits, mem.Ty.Size*8)
				continue
			}
			sz := mem.Ty.Size
			if !ty.IsPacked {
				if bits/(sz*8) != (bits+mem.BitWidth-1)/(sz*8) {
					bits = alignTo(bits, sz*8)
				}
			}

			mem.Offset = alignDown(bits/8, sz)
			mem.BitOffset = bits % (sz * 8)
			bits += mem.BitWidth
			continue
		}

		if ty.IsPacked {
			bits = alignTo(bits, 8)
		} else {
			bits = alignTo(bits, mem.Ty.Align*8)
		}

		mem.Offset = bits / 8
		bits += mem.Ty.Size * 8
	}

	cur.Next = nil
	ty.Members = head.Next
	if altAlign != 0 {
		ty.Align = altAlign
	}
	if ty.IsPacked && altAlign == 0 {
		ty.Size = alignTo(bits, 8) / 8
	} else {
		ty.Size = alignTo(bits, ty.Align*8) / 8
	}
	return ty
}

func attrAligned(tok *Token, align *int64) {
	for lst := tok.AttrNext; lst != nil; lst = lst.AttrNext {
		if lst.isEqual("aligned") || lst.isEqual("__aligned__") {
			var tok2 *Token
			if consume(&tok2, lst.Next, "(") {
				align2 := constExpr(&tok2, tok2)
				*align = int64(math.Max(float64(*align), float64(align2)))
				continue
			}
			*align = int64(math.Max(float64(*align), 16))
		}
	}
}

func attrPacked(tok *Token, ty *CType, allowBAttr bool) {
	for lst := tok.AttrNext; lst != nil; lst = lst.AttrNext {
		if lst.isEqual("packed") || lst.isEqual("__packed__") {
			if !allowBAttr && lst.Kind == TK_BATTR {
				continue
			}
			ty.IsPacked = true
			continue
		}
	}
}

func declarator2(rest **Token, tok *Token, basety *CType, name **Token, align *int64) *CType {
	attrAligned(tok, align)
	ty := declarator(&tok, tok, basety, name)
	attrAligned(tok, align)
	*rest = tok
	return ty
}

// union-decl = struct-union-decl
func unionDecl(ty *CType, altAlign int64) *CType {
	head := Member{}
	cur := &head
	for mem := ty.Members; mem != nil; mem = mem.Next {
		if !mem.IsBitfield || mem.Name != nil {
			cur.Next = mem
			cur = cur.Next
			if !ty.IsPacked {
				altAlign = int64(math.Max(float64(altAlign), math.Max(float64(mem.AltAlign), float64(mem.Ty.Align))))
			} else if mem.AltAlign != 0 && mem.Ty.Kind != TY_STRUCT && mem.Ty.Kind != TY_UNION {
				altAlign = int64(math.Max(float64(altAlign), float64(mem.AltAlign)))
			}
		}
		sz := mem.Ty.Size
		if mem.IsBitfield {
			sz = alignTo(mem.BitWidth, 8) / 8
		}

		ty.Size = int64(math.Max(float64(ty.Size), float64(sz)))
	}

	cur.Next = nil
	ty.Members = head.Next
	if altAlign != 0 {
		ty.Align = altAlign
	}
	ty.Size = alignTo(ty.Size, ty.Align)
	return ty
}

// Find a struct member by name.
func getStructMember(ty *CType, tok *Token) *Member {
	for mem := ty.Members; mem != nil; mem = mem.Next {
		// Anonymous struct member
		if (mem.Ty.Kind == TY_STRUCT || mem.Ty.Kind == TY_UNION) && mem.Name == nil && getStructMember(mem.Ty, tok) != nil {
			return mem
		}

		// Regular struct member
		if mem.Name != nil && mem.Name.isEqual(tok.getText()) {
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
		applyCvQualifier(node, ty)
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
	fnTy := funcType(ty, tok)

	if tok.isEqual("...") && consume(rest, tok.Next, ")") {
		fnTy.IsVariadic = true
		return fnTy
	}

	if tok.isEqual("void") && consume(rest, tok.Next, ")") {
		return fnTy
	}

	if consume(rest, tok, ")") {
		if opt_std < STD_C23 {
			fnTy.IsOldStyle = true
		}
		return fnTy
	}
	if !tok.isTypename() {
		return funcParamsOldStyle(rest, tok, fnTy)
	}

	head := Obj{}
	cur := &head
	var expr *AstNode = nil

	enterScope()
	fnTy.Scopes = scope

	for commaList(rest, &tok, ")", cur != &head) {
		if tok.isEqual("...") {
			fnTy.IsVariadic = true
			*rest = skip(tok.Next, ")")
			break
		}

		ty2 := declspec(&tok, tok, nil)

		var name *Token = nil
		ty2 = declarator(&tok, tok, ty2, &name)
		chainExpr(&expr, computeVlaSize(ty2, tok))

		if ty2.Kind == TY_ARRAY || ty2.Kind == TY_VLA {
			// "array of T" is converted to "pointer to T" only in the parameter
			// context. For example, *argv[] is converted to **argv by this.
			ty3 := pointerTo(ty2.Base)
			ty3.IsAtomic = ty2.IsAtomic
			ty3.IsConst = ty2.IsConst
			ty3.IsVolatile = ty2.IsVolatile
			ty3.IsRestrict = ty2.IsRestrict
			ty2 = ty3
		} else if ty2.Kind == TY_FUNC {
			// Likewise, a function is converted to a pointer to a function
			// only in the parameter context.
			ty2 = pointerTo(ty2)
		}

		varName := ""
		if name != nil {
			varName = name.getIdent()
		}
		cur.ParamNext = newLocalVar(varName, ty2)
		cur = cur.ParamNext
	}

	leaveScope()

	expr.addType()
	fnTy.ParamList = head.ParamNext
	fnTy.PreCalc = expr
	return fnTy
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
func arrayDimensions(rest **Token, tok *Token, ty *CType) *CType {
	if consume(&tok, tok, "]") || (tok.isEqual("*") && consume(&tok, tok.Next, "]")) {
		if tok.isEqual("[") {
			ty = arrayDimensions(&tok, tok.Next, ty)
		}
		*rest = tok
		return arrayOf(ty, -1)
	}

	expr := assign(&tok, tok)
	expr.addType()
	if !expr.Ty.isInteger() {
		errorTok(tok, "size of array not integer")
	}
	tok = skip(tok, "]")
	if tok.isEqual("[") {
		ty = arrayDimensions(&tok, tok.Next, ty)
	}
	*rest = tok

	arrayLength := int64(0)
	if ty.Kind != TY_VLA && expr.isConstExpr(&arrayLength) {
		return arrayOf(ty, arrayLength)
	}

	if scope.Parent == nil {
		errorTok(tok, "variably-modified type at file scope")
	}
	return vlaOf(ty, expr)
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

	if consume(&tok, tok, "[") {
		if tok.Kind == TK_KEYWORD {
			start := tok
			pointerQualifiers(&tok, tok, &CType{})
			consume(&tok, tok, "static")
			ty2 := arrayDimensions(rest, tok, ty)
			dummy := &Token{}
			pointerQualifiers(&dummy, start, ty2)
			return ty2
		}
		return arrayDimensions(rest, tok, ty)
	}

	*rest = tok
	return ty
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
func pointers(rest **Token, tok *Token, ty *CType) *CType {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)
		pointerQualifiers(&tok, tok, ty)
	}

	*rest = tok
	return ty
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(rest **Token, tok *Token, ty *CType, nameToken **Token) *CType {
	ty = pointers(&tok, tok, ty)

	if consume(&tok, tok, "(") {
		if tok.isTypename() || tok.isEqual(")") {
			return funcParams(rest, tok, ty)
		}

		ty = typeSuffix(rest, skipParen(tok), ty)
		t := &Token{}
		return declarator(&t, tok, ty, nameToken)
	}

	if nameToken != nil && tok.Kind == TK_IDENT {
		*nameToken = tok
		tok = tok.Next
	}

	return typeSuffix(rest, tok, ty)
}

func (tok *Token) isEnd() bool {
	return tok.isEqual("}") || (tok.isEqual(",") && tok.Next.isEqual("}"))
}

// type-name = declspec abstract-declarator
func typeName(rest **Token, tok *Token) *CType {
	ty := declspec(&tok, tok, nil)
	return declarator(rest, tok, ty, nil)
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token, basety *CType, attr *VarAttr) *AstNode {
	var expr *AstNode = nil

	first := true
	for ; commaList(rest, &tok, ";", !first); first = false {
		var name *Token = nil
		altAlign := int64(0)
		if attr != nil {
			altAlign = attr.Align
		}
		ty := declarator2(&tok, tok, basety, &name, &altAlign)
		if ty.Kind == TY_FUNC {
			if name == nil {
				errorTok(tok, "function name omitted")
			}
			funcPrototype(ty, attr, name)
			continue
		}
		if ty.Kind == TY_VOID {
			errorTok(tok, "variable declared as void")
		}
		if name == nil {
			errorTok(tok, "variable name omitted")
		}

		// Generate code for computing a VLA size. We need to do this
		// even if ty is not VLA because ty may be a pointer to VLA
		// (e.g. int (*foo)[n][m] where n and m are variables.)
		chainExpr(&expr, computeVlaSize(ty, tok))

		if attr != nil && attr.IsStatic {
			if ty.Kind == TY_VLA {
				errorTok(tok, "variable length arrays cannot be 'static'")
			}

			// static local variable
			variable := newAnonymousGlobalVariable(ty)
			variable.IsTls = attr.IsTls
			if altAlign != 0 {
				variable.Align = altAlign
			}
			pushScope(name.getIdent()).Variable = variable

			if attr.IsConstExpr {
				if !tok.isEqual("=") {
					errorTok(tok, "constexpr variable not initialized")
				}
				constExprInitializer(&tok, tok.Next, variable, variable)
			}
			if tok.isEqual("=") {
				globalVarInitializer(&tok, tok.Next, variable)
			}
			continue
		}

		if ty.Kind == TY_VLA {
			if tok.isEqual("=") {
				errorTok(tok, "variable-sized object may not be initialized")
			}
			// Variable length arrays (VLAs) are translated to alloca() calls.
			// For example, `int x[n+2]` is translated to `tmp = n + 2,
			// x = alloca(tmp)`.
			v := newLocalVar(name.getIdent(), ty)

			chainExpr(&expr, newVLA(newVarNode(ty.VlaSize, name), v, int64(math.Max(float64(altAlign), 16))))

			v.VlaNext = CurrentVLA
			CurrentVLA = v
			FnUseVLA = true
			continue
		}

		variable := newLocalVar(name.getIdent(), ty)
		if altAlign != 0 {
			variable.Align = altAlign
		}

		if attr != nil && attr.IsConstExpr {
			if !tok.isEqual("=") {
				errorTok(tok, "constexpr variable not initialized")
			}
			initVar := newAnonymousGlobalVariable(ty)
			constExprInitializer(&tok, tok.Next, initVar, variable)
			chainExpr(&expr, newBinary(ND_ASSIGN, newVarNode(variable, tok), newVarNode(initVar, tok), tok))
			continue
		}

		if tok.isEqual("=") {
			chainExpr(&expr, localVarInitializer(&tok, tok.Next, variable))
		}

		if variable.Ty.Size < 0 {
			errorTok(name, "variable has incomplete type")
		}
		if variable.Ty.Kind == TY_VOID {
			errorTok(name, "variable declared as void")
		}
	}

	return expr
}

// 从 []int8 缓冲区按小端顺序读取 sz 字节（1、2、4、8），拼成一个 uint64 值
func readBuf(buf *[]int8, offset int64, sz int64) uint64 {
	if sz != 1 && sz != 2 && sz != 4 && sz != 8 {
		panic("unsupported size")
	}

	var val uint64 = 0
	for i := int64(0); i < sz; i++ {
		b := uint64((*buf)[offset+i]) & 0xff // 转成无符号，避免符号扩展
		val |= b << (8 * i)                  // 小端拼接
	}
	return val
}

// 将 uint64 按小端顺序写入 []int8
func writeBuf(buf *[]int8, offset int64, val uint64, sz int64) {
	if sz != 1 && sz != 2 && sz != 4 && sz != 8 {
		panic("writeBuf: unsupported size")
	}
	for i := int64(0); i < sz; i++ {
		(*buf)[offset+i] = int8(val >> (8 * i))
	}
}

func writeGlobalVarData(cur *Relocation, init *Initializer, ty *CType, buf *[]int8, offset int64, kind EvalKind) *Relocation {
	if ty.Kind == TY_ARRAY {
		sz := ty.Base.Size
		for i := int64(0); i < ty.ArrayLength; i++ {
			cur = writeGlobalVarData(cur, init.Children[i], ty.Base, buf, offset+sz*i, kind)
		}
		return cur
	}

	if ty.Kind == TY_STRUCT {
		for mem := ty.Members; mem != nil; mem = mem.Next {
			if mem.IsBitfield {
				expr := init.Children[mem.Index].Expr
				if expr == nil {
					continue
				}
				expr.addType()

				loc := offset + mem.Offset
				oldVal := readBuf(buf, loc, mem.Ty.Size)
				newVal := eval(expr)
				mask := int64((1 << mem.BitWidth) - 1)
				combined := uint64(oldVal) | uint64((newVal&mask)<<mem.BitOffset)
				writeBuf(buf, loc, combined, mem.Ty.Size)
			} else {
				cur = writeGlobalVarData(cur, init.Children[mem.Index], mem.Ty, buf, offset+mem.Offset, kind)
			}
		}
		return cur
	}

	if ty.Kind == TY_UNION {
		if init.Member == nil {
			return cur
		}
		return writeGlobalVarData(cur, init.Children[init.Member.Index], init.Member.Ty, buf, offset, kind)
	}

	if init.Expr == nil {
		return cur
	}

	init.Expr.addType()

	if ty.Kind == TY_FLOAT {
		val := Float32ToInt8Slice(evalDouble(newCast(init.Expr, TyFloat)).ToFloat32())
		copy((*buf)[offset:offset+int64(len(val))], val)
		return cur
	}

	if ty.Kind == TY_DOUBLE {
		val := Float64ToInt8Slice(evalDouble(newCast(init.Expr, TyDouble)).ToFloat64())
		copy((*buf)[offset:offset+int64(len(val))], val)
		return cur
	}

	if ty.Kind == TY_LDOUBLE {
		val := evalDouble(newCast(init.Expr, TyLDouble))
		val = val.ToFloat80()
		copy((*buf)[offset:offset+int64(len(val.ToInt8Slice()))], val.ToInt8Slice())
		return cur
	}

	ctx := EvalContext{
		Kind: kind,
	}

	val := eval2(init.Expr, &ctx)

	if ctx.Label == nil {
		writeBuf(buf, offset, uint64(val), ty.Size)
		return cur
	}

	rel := &Relocation{}
	rel.Offset = offset
	rel.Label = ctx.Label
	rel.Addend = val
	cur.Next = rel
	return cur.Next
}

func constExprInitializer(rest **Token, tok *Token, initVar *Obj, v *Obj) {
	init := initializer(rest, tok, initVar.Ty, &initVar.Ty)

	head := Relocation{}
	buf := make([]int8, initVar.Ty.Size)
	writeGlobalVarData(&head, init, initVar.Ty, &buf, 0, EV_CONST)
	initVar.InitData = buf
	v.ConstExprData = buf
	initVar.Rel = head.Next
	v.Ty = init.Ty
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
	writeGlobalVarData(&head, init, variable.Ty, &buf, 0, EV_LABEL)
	variable.InitData = buf
	variable.Rel = head.Next
}

// Returns true if a given token represents a type.
func (tok *Token) isTypename() bool {
	if opt_std == STD_NONE || opt_std >= STD_C23 {
		TypeNames["typeof"] = struct{}{}
	}
	if opt_std >= STD_C23 {
		TypeNames["constexpr"] = struct{}{}
		TypeNames["typeof_unqual"] = struct{}{}
	}
	name := tok.getText()
	_, ok := TypeNames[name]
	if ok {
		return true
	}

	return findTypeDef(tok) != nil
}

func atomicOp(binary *AstNode, returnOld bool) *AstNode {
	// ({
	//   T *addr = &obj; T old = *addr; T new;
	//   do {
	//    new = old op val;
	//   } while (!atomic_compare_exchange_strong(addr, &old, new));
	//
	//   return_old ? old : new;
	// })
	tok := binary.Tok
	head := AstNode{}
	cur := &head

	addr := newLocalVar("", pointerTo(binary.Lhs.Ty))
	val := newLocalVar("", binary.Rhs.Ty)
	old := newLocalVar("", binary.Lhs.Ty)
	new := newLocalVar("", binary.Lhs.Ty)

	cur.Next = newUnary(ND_EXPR_STMT, newBinary(ND_ASSIGN, newVarNode(addr, tok), newUnary(ND_ADDR, binary.Lhs, tok), tok), tok)
	cur = cur.Next

	cur.Next = newUnary(ND_EXPR_STMT, newBinary(ND_ASSIGN, newVarNode(val, tok), binary.Rhs, tok), tok)
	cur = cur.Next

	cur.Next = newUnary(ND_EXPR_STMT, newBinary(ND_ASSIGN, newVarNode(old, tok), newUnary(ND_DEREF, newVarNode(addr, tok), tok), tok), tok)
	cur = cur.Next

	loop := newNode(ND_DO, tok)
	loop.BreakLabel = newUniqueName()
	loop.ContinueLabel = newUniqueName()

	body := newBinary(ND_ASSIGN, newVarNode(new, tok), newBinary(binary.Kind, newVarNode(old, tok), newVarNode(val, tok), tok), tok)

	loop.Then = newNode(ND_BLOCK, tok)
	loop.Then.Body = newUnary(ND_EXPR_STMT, body, tok)

	cas := newNode(ND_CAS, tok)
	cas.CasAddr = newVarNode(addr, tok)
	cas.CasOld = newUnary(ND_ADDR, newVarNode(old, tok), tok)
	cas.CasNew = newVarNode(new, tok)
	loop.Cond = newUnary(ND_NOT, cas, tok)

	cur.Next = loop
	cur = cur.Next

	if returnOld {
		cur.Next = newUnary(ND_EXPR_STMT, newVarNode(old, tok), tok)
	} else {
		cur.Next = newUnary(ND_EXPR_STMT, newVarNode(new, tok), tok)
	}

	node := newNode(ND_STMT_EXPR, tok)
	node.Body = head.Next
	return node
}

/*
 * stmt = "return" expr? ";"
 *	    | "if" "(" expr ")" stmt ("else" stmt)?
 *      | "switch" "(" expr ")" stmt
 *	    | "case" const-expr ("..." const-expr)? ":" stmt
 *	    | "default" ":" stmt
 *	    | "for" "(" expr-stmt expr? ";" expr? ")" stmt
 *	    | "while" "(" expr ")" stmt
 *	    | "do" stmt "while" "(" expr ")" ";"
 *      | "__asm__" asm-stmt
 *      | "goto" (ident | "*" expr) ";"
 *      | "break" ";"
 *	    | "continue" ";"
 *	    | ident ":" stmt
 *	    | "{" compound-stmt
 *	    | expr-stmt
 */
func stmt(rest **Token, tok *Token, chained bool) *AstNode {
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
		node.Then = stmt(&tok, tok, true)
		if tok.isEqual("else") {
			node.Else = stmt(&tok, tok.Next, true)
		}
		*rest = tok
		return node
	}

	if tok.isEqual("switch") {
		node := newNode(ND_SWITCH, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		node.Cond.addType()
		if !node.Cond.Ty.isInteger() {
			errorTok(tok, "controlling expression not integer")
		}
		tok = skip(tok, ")")

		sw := currentSwitch
		currentSwitch = node

		brk := breakLabel
		node.BreakLabel = newUniqueName()
		breakLabel = node.BreakLabel

		vla := BreakVLA
		BreakVLA = CurrentVLA

		node.Then = stmt(rest, tok, true)

		currentSwitch = sw
		breakLabel = brk
		BreakVLA = vla
		return node
	}

	if tok.isEqual("case") {
		if currentSwitch == nil {
			errorTok(tok, "stray case")
		}
		if CurrentVLA != BreakVLA {
			errorTok(tok, "jump crosses VLA initialization")
		}

		node := newNode(ND_CASE, tok)
		node.Label = newUniqueName()

		begin := constExpr(&tok, tok.Next)
		end := int64(0)

		// [GNU] Case ranges, e.g. "case 1 ... 5:"
		if tok.isEqual("...") {
			end = constExpr(&tok, tok.Next)
		} else {
			end = begin
		}

		condTy := currentSwitch.Cond.Ty
		if condTy.Size <= 4 {
			if !condTy.IsUnsigned {
				begin = int64(int32(begin))
				end = int64(int32(end))
			} else {
				begin = int64(uint32(begin))
				end = int64(uint32(end))
			}
		}

		if (!condTy.IsUnsigned && (end < begin)) || (condTy.IsUnsigned && uint64(end) < uint64(begin)) {
			errorTok(tok, "empty case range specified")
		}

		tok = skip(tok, ":")
		if chained {
			node.Lhs = stmt(rest, tok, true)
		} else {
			*rest = tok
		}
		node.Begin = begin
		node.End = end
		node.CaseNext = currentSwitch.CaseNext
		currentSwitch.CaseNext = node
		return node
	}

	if tok.isEqual("default") {
		if currentSwitch == nil {
			errorTok(tok, "stray default")
		}
		if CurrentVLA != BreakVLA {
			errorTok(tok, "jump crosses VLA initialization")
		}
		node := newNode(ND_CASE, tok)
		node.Label = newUniqueName()

		tok = skip(tok.Next, ":")
		if chained {
			node.Lhs = stmt(rest, tok, true)
		} else {
			*rest = tok
		}
		currentSwitch.DefaultCase = node
		return node
	}

	if tok.isEqual("for") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")

		node.TargetVLA = CurrentVLA
		enterTmpScope()

		if tok.isTypename() {
			basety := declspec(&tok, tok, nil)
			expr := declaration(&tok, tok, basety, nil)
			if expr != nil {
				node.Init = newUnary(ND_EXPR_STMT, expr, tok)
			}
		} else if tok.isEqual("_Static_assert") {
			staticAssertion(&tok, tok.Next)
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

		loopBody(rest, tok, node)

		node.TopVLA = CurrentVLA
		CurrentVLA = node.TargetVLA
		leaveScope()
		return node
	}

	if tok.isEqual("while") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")

		loopBody(rest, tok, node)
		return node
	}

	if tok.isEqual("do") {
		node := newNode(ND_DO, tok)

		loopBody(&tok, tok.Next, node)

		tok = skip(tok, "while")
		tok = skip(tok, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")
		*rest = skip(tok, ";")
		return node
	}

	if tok.Kind == TK_KEYWORD && (tok.isEqual("asm") || tok.isEqual("__asm") || tok.isEqual("__asm__")) {
		return asmStmt(rest, tok)
	}

	if tok.isEqual("goto") {
		if tok.Next.isEqual("*") {
			// [GNU] `goto *ptr` jumps to the address specified by `ptr`.
			node := newNode(ND_GOTO_EXPR, tok)
			node.Lhs = expr(&tok, tok.Next.Next)
			*rest = skip(tok, ";")
			return node
		}

		node := newNode(ND_GOTO, tok)
		node.Label = tok.Next.getIdent()
		node.GotoNext = gotos
		node.TopVLA = CurrentVLA
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
		node.TargetVLA = BreakVLA
		node.TopVLA = CurrentVLA
		*rest = skip(tok.Next, ";")
		return node
	}

	if tok.isEqual("continue") {
		if continueLabel == "" {
			errorTok(tok, "stray continue")
		}
		node := newNode(ND_GOTO, tok)
		node.UniqueLabel = continueLabel
		node.TargetVLA = BreakVLA
		node.TopVLA = CurrentVLA
		*rest = skip(tok.Next, ";")
		return node
	}

	if tok.Kind == TK_IDENT && tok.Next.isEqual(":") {
		node := newNode(ND_LABEL, tok)
		node.Label = tok.getIdent()

		tok = tok.Next.Next
		if chained {
			node.Lhs = stmt(rest, tok, true)
		} else {
			*rest = tok
		}
		node.UniqueLabel = newUniqueName()
		node.GotoNext = labels
		node.TopVLA = CurrentVLA
		labels = node

		return node
	}

	if tok.isEqual("{") {
		return compoundStmt(rest, tok.Next, ND_BLOCK)
	}

	return exprStmt(rest, tok)
}

/*
 * This function matches gotos or labels-as-values with labels.
 *
 * We cannot resolve gotos as we parse a function because gotos
 * can refer a label that appears later in the function.
 * So, we need to do this after we parse the entire function.
 */
func resolveGotoLabels() {
	for x := gotos; x != nil; x = x.GotoNext {
		dest := labels
		for ; dest != nil; dest = dest.GotoNext {
			if x.Label == dest.Label {
				break
			}
		}

		if dest == nil {
			errorTok(x.Tok.Next, "use of undeclared label")
		}

		x.UniqueLabel = dest.UniqueLabel
		if dest.TopVLA == nil {
			continue
		}

		vla := x.TopVLA
		for ; vla != nil; vla = vla.VlaNext {
			if vla == dest.TopVLA {
				break
			}
		}

		if vla == nil {
			errorTok(x.Tok.Next, "jump crosses VLA initialization")
		}

		x.TargetVLA = vla
	}

	labels = nil
	gotos = nil
}

func applyCvQualifier(node *AstNode, ty2 *CType) {
	node.addType()
	ty := node.Ty
	if (!ty.IsConst && ty2.IsConst) || (!ty.IsVolatile && ty2.IsVolatile) {
		node.Ty = newQualifiedType(ty)
		node.Ty.IsConst = ty.IsConst || ty2.IsConst
		node.Ty.IsVolatile = ty.IsVolatile || ty2.IsVolatile
	}
}

// compound-stmt = (typedef | declaration | stmt)* "}"
func compoundStmt(rest **Token, tok *Token, kind AstNodeKind) *AstNode {
	node := newNode(kind, tok)
	head := AstNode{}
	cur := &head

	node.TargetVLA = CurrentVLA

	enterScope()

	for ; !tok.isEqual("}"); cur.addType() {
		if tok.isEqual("_Static_assert") {
			staticAssertion(&tok, tok.Next)
			continue
		}

		if tok.isTypename() && !tok.Next.isEqual(":") {
			attr := VarAttr{}
			basety := declspec(&tok, tok, &attr)

			if attr.IsTypeDef {
				expr := parseTypeDef(&tok, tok, basety, &attr)
				if expr != nil {
					cur.Next = newUnary(ND_EXPR_STMT, expr, tok)
					cur = cur.Next
				}
				continue
			}

			if attr.IsExtern {
				tok = globalDeclaration(tok, basety, &attr)
				continue
			}

			expr := declaration(&tok, tok, basety, &attr)
			if expr != nil {
				cur.Next = newUnary(ND_EXPR_STMT, expr, tok)
				cur = cur.Next
			}
			continue
		}

		cur.Next = stmt(&tok, tok, false)
		cur = cur.Next
	}

	node.TopVLA = CurrentVLA
	CurrentVLA = node.TargetVLA
	leaveScope()

	if kind == ND_STMT_EXPR && cur.Kind == ND_EXPR_STMT {
		cur.Lhs.addType()
		ty := cur.Lhs.Ty
		if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
			v := newLocalVar("", ty)
			expr := newBinary(ND_ASSIGN, newVarNode(v, tok), cur.Lhs, tok)
			chainExpr(&expr, newVarNode(v, tok))
			cur.Lhs = expr
		}
	}

	node.Body = head.Next
	*rest = tok.Next

	return node
}

// expr-stmt = expr? ";"
func exprStmt(rest **Token, tok *Token) *AstNode {
	if consume(rest, tok, ";") {
		return newNode(ND_BLOCK, tok)
	}

	node := newNode(ND_EXPR_STMT, tok)
	node.Lhs = expr(&tok, tok)
	*rest = skip(tok, ";")
	return node
}

func evalDouble(node *AstNode) FloatConst {
	if node.Ty.isInteger() {
		panic("node.Ty.isInteger()")
	}
	if evalRecover != nil && *evalRecover {
		return FloatConst64{float64(boolToInt(false))}
	}

	switch node.Kind {
	case ND_ADD:
		return evalDouble(node.Lhs).Add(evalDouble(node.Rhs))
	case ND_SUB:
		return evalDouble(node.Lhs).Sub(evalDouble(node.Rhs))
	case ND_MUL:
		return evalDouble(node.Lhs).Mul(evalDouble(node.Rhs))
	case ND_DIV:
		return evalDouble(node.Lhs).Div(evalDouble(node.Rhs))
	case ND_POS:
		return evalDouble(node.Lhs)
	case ND_NEG:
		return evalDouble(node.Lhs).Neg()
	case ND_COND:
		if eval(node.Cond) != 0 {
			return evalDouble(node.Then)
		} else {
			return evalDouble(node.Else)
		}
	case ND_COMMA:
		evalVoid(node.Lhs)
		return evalDouble(node.Rhs)
	case ND_CAST:
		if node.Lhs.Ty.isFloat() {
			if node.Ty.Size == 4 {
				return FloatConst64{float64(evalDouble(node.Lhs).ToFloat32())}
			}
			if node.Lhs.Ty.Kind == TY_LDOUBLE && node.Ty.Kind == TY_DOUBLE {
				return FloatConst64{evalDouble(node.Lhs).ToFloat64()}
			}
			return evalDouble(node.Lhs)
		}
		// bug here
		if node.Lhs.Ty.Size == 8 && node.Lhs.Ty.IsUnsigned {
			if node.Ty == TyLDouble {
				text := fmt.Sprintf("%d", uint64(eval(node.Lhs)))
				// 用 big.Float 构造高精度数值
				f, _, err := new(big.Float).Parse(text, 10)
				if err != nil {
					panic(err)
				}
				f.SetPrec(256)
				value := FloatConst80{f}
				return value
			}
			return FloatConst64{float64(uint64(eval(node.Lhs)))}
		}
		return FloatConst64{float64(eval(node.Lhs))}
	case ND_NUM:
		return node.FloatValue
	}

	if node.Kind == ND_VAR && node.Variable.ConstExprData != nil {
		// for i := 0; i < len(node.Variable.ConstExprData); i++ {
		// 	print(node.Variable.ConstExprData[i], " ")
		// }
		// println()
		return readDoubleBuf(&node.Variable.ConstExprData, node.Variable.Ty)
	}

	if node.Kind == ND_MEMBER || node.Kind == ND_DEREF {
		data := evalConstExprAgg(node)
		if data == nil {
			return FloatConst64{0}
		}
		return readDoubleBuf(&data, node.Ty)
	}

	return FloatConst64{float64(evalError(node.Tok, "not a compile-time constant"))}
}

func staticAssertion(rest **Token, tok *Token) {
	tok = skip(tok, "(")
	result := constExpr(&tok, tok)
	if result == 0 {
		errorTok(tok, "static assertion failed")
	}

	if tok.isEqual(",") {
		if tok.Next.Kind != TK_STR {
			errorTok(tok, "expected string literal")
		}
		tok = tok.Next.Next
	}

	tok = skip(tok, ")")
	*rest = skip(tok, ";")
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
func eval2(node *AstNode, ctx *EvalContext) int64 {
	if ctx.Kind == EV_CONST && node.Ty.isFloat() {
		panic("ctx.Kind == EV_CONST && node.Ty.isFloat()")
	}
	if evalRecover != nil && *evalRecover {
		return 0
	}
	switch node.Kind {
	case ND_ADD:
		return eval2(node.Lhs, ctx) + eval(node.Rhs)
	case ND_SUB:
		return eval2(node.Lhs, ctx) - eval(node.Rhs)
	case ND_MUL:
		return eval(node.Lhs) * eval(node.Rhs)
	case ND_DIV:
		lhs := eval(node.Lhs)
		rhs := eval(node.Rhs)
		if rhs == 0 {
			return evalError(node.Rhs.Tok, "division by zero during constant evaluation")
		}
		if rhs == -1 && !node.Ty.IsUnsigned {
			if node.Ty.Size == 4 && lhs == math.MinInt32 {
				return math.MinInt32
			}
			if node.Ty.Size == 8 && lhs == math.MinInt64 {
				return math.MinInt64
			}
		}
		if node.Ty.IsUnsigned {
			return int64(uint64(lhs) / uint64(rhs))
		}
		return lhs / rhs
	case ND_POS:
		return eval(node.Lhs)
	case ND_NEG:
		if node.Ty.Size == 4 {
			if node.Ty.IsUnsigned {
				return int64(uint32(-eval(node.Lhs)))
			}
			return int64(int32(-eval(node.Lhs)))
		}
		return -eval(node.Lhs)
	case ND_MOD:
		lhs := eval(node.Lhs)
		rhs := eval(node.Rhs)
		if rhs == 0 {
			return evalError(node.Rhs.Tok, "remainder by zero during constant evaluation")
		}
		if rhs == -1 && !node.Ty.IsUnsigned && node.Ty.Size == 8 && lhs == math.MinInt64 {
			return 0
		}
		if node.Ty.IsUnsigned {
			return int64(uint64(lhs) % uint64(rhs))
		}
		return lhs % rhs
	case ND_BITAND:
		return eval(node.Lhs) & eval(node.Rhs)
	case ND_BITOR:
		return eval(node.Lhs) | eval(node.Rhs)
	case ND_BITXOR:
		return eval(node.Lhs) ^ eval(node.Rhs)
	case ND_SHL:
		if node.Ty.Size == 4 {
			if node.Ty.IsUnsigned {
				return int64(uint32(eval(node.Lhs)) << uint32(eval(node.Rhs)))
			}
			return int64(int32(eval(node.Lhs)) << int32(eval(node.Rhs)))
		}
		return eval(node.Lhs) << eval(node.Rhs)
	case ND_SHR:
		if node.Ty.Size == 4 {
			return int64(uint32(eval(node.Lhs)) >> uint32(eval(node.Rhs)))
		}
		return int64(uint64(eval(node.Lhs)) >> uint64(eval(node.Rhs)))
	case ND_SAR:
		if node.Ty.Size == 4 {
			return int64(int32(eval(node.Lhs)) >> int32(eval(node.Rhs)))
		}
		return eval(node.Lhs) >> eval(node.Rhs)
	case ND_EQ:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Eq(evalDouble(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) == eval(node.Rhs)))
	case ND_NE:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Ne(evalDouble(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) != eval(node.Rhs)))
	case ND_LT:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Lt(evalDouble(node.Rhs))))
		}
		if node.Lhs.Ty.IsUnsigned {
			return int64(boolToInt(uint64(eval(node.Lhs)) < uint64(eval(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) < eval(node.Rhs)))
	case ND_LE:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Le(evalDouble(node.Rhs))))
		}
		if node.Lhs.Ty.IsUnsigned {
			return int64(boolToInt(uint64(eval(node.Lhs)) <= uint64(eval(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) <= eval(node.Rhs)))
	case ND_GT:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Gt(evalDouble(node.Rhs))))
		}
		if node.Lhs.Ty.IsUnsigned {
			return int64(boolToInt(uint64(eval(node.Lhs)) > uint64(eval(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) > eval(node.Rhs)))
	case ND_GE:
		if node.Lhs.Ty.isFloat() {
			return int64(boolToInt(evalDouble(node.Lhs).Ge(evalDouble(node.Rhs))))
		}
		if node.Lhs.Ty.IsUnsigned {
			return int64(boolToInt(uint64(eval(node.Lhs)) >= uint64(eval(node.Rhs))))
		}
		return int64(boolToInt(eval(node.Lhs) >= eval(node.Rhs)))
	case ND_COND:
		if eval(node.Cond) != 0 {
			return eval2(node.Then, ctx)
		} else {
			return eval2(node.Else, ctx)
		}
	case ND_COMMA:
		evalVoid(node.Lhs)
		return eval2(node.Rhs, ctx)
	case ND_NOT:
		return int64(boolToInt(eval(node.Lhs) == 0))
	case ND_BITNOT:
		if node.Ty.Size == 4 {
			if node.Ty.IsUnsigned {
				return int64(uint32(^eval(node.Lhs)))
			}
			return int64(int32(^eval(node.Lhs)))
		}
		return ^eval(node.Lhs)
	case ND_LOGAND:
		return int64(boolToInt(eval(node.Lhs) != 0 && eval(node.Rhs) != 0))
	case ND_LOGOR:
		return int64(boolToInt(eval(node.Lhs) != 0 || eval(node.Rhs) != 0))
	case ND_CAST:
		if node.Ty.Kind == TY_BOOL {
			if node.Lhs.Kind == ND_VAR && (node.Lhs.Ty.Kind == TY_ARRAY || node.Lhs.Ty.Kind == TY_VLA) {
				return 1
			}
			if node.Lhs.Ty.isFloat() {
				return int64(boolToInt(!evalDouble(node.Lhs).IsZero()))
			}
			return int64(boolToInt(eval2(node.Lhs, ctx) != 0))
		}

		if node.Lhs.Ty.isFloat() {
			if node.Ty.Size == 8 && node.Ty.IsUnsigned {
				return int64(evalDouble(node.Lhs).ToUInt64())
			}
			return evalDouble(node.Lhs).ToInt64()
		}

		val := eval2(node.Lhs, ctx)
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
	case ND_NUM:
		return node.Value
	}

	if node.Kind == ND_ADDR && node.Lhs.Kind == ND_DEREF {
		return eval2(node.Lhs.Lhs, ctx)
	}

	if node.Kind == ND_DEREF && node.Lhs.Kind == ND_ADDR {
		return eval2(node.Lhs.Lhs, ctx)
	}

	if ctx.Kind == EV_LABEL {
		switch node.Kind {
		case ND_ADDR, ND_DEREF:
			return eval2(node.Lhs, ctx)
		case ND_MEMBER:
			return eval2(node.Lhs, ctx) + node.Member.Offset
		case ND_LABEL_VAL:
			ctx.Label = &node.UniqueLabel
			return 0
		case ND_VAR:
			if node.Variable.IsLocal {
				return evalError(node.Tok, "not a compile-time constant")
			}
			ctx.Label = &node.Variable.Name
			return 0
		}
		return evalError(node.Tok, "invalid initializer")
	}

	if ctx.Kind == EV_AGG {
		if node.Kind == ND_DEREF {
			return eval2(node.Lhs, ctx)
		}
		if node.Kind == ND_MEMBER {
			return eval2(node.Lhs, ctx) + node.Member.Offset
		}
		if node.Kind == ND_VAR && node.Variable.ConstExprData != nil {
			ctx.Var = node.Variable
			return 0
		}
		return evalError(node.Tok, "not a compile-time constant")
	}

	if ctx.Kind == EV_CONST {
		if node.Kind == ND_VAR && node.Variable.ConstExprData != nil {
			return int64(readBuf(&node.Variable.ConstExprData, 0, node.Variable.Ty.Size))
		}

		if node.Kind == ND_MEMBER || node.Kind == ND_DEREF {
			data := evalConstExprAgg(node)
			if data == nil {
				return 0
			}
			val := readBuf(&data, 0, node.Ty.Size)
			if node.isBitField() {
				val <<= 64 - node.Member.BitWidth - node.Member.BitOffset
				if node.Ty.IsUnsigned {
					return int64(uint64(val) >> (64 - node.Member.BitWidth))
				}
				return int64(val) >> (64 - node.Member.BitWidth)
			}

			return int64(val)
		}
	}

	return evalError(node.Tok, "not a compile-time constant")
}

func eval(node *AstNode) int64 {
	return eval2(node, &EvalContext{Kind: EV_CONST})
}

func evalConstExprAgg(node *AstNode) []int8 {
	ctx := EvalContext{Kind: EV_AGG}
	ofs := eval2(node, &ctx)
	if evalRecover != nil && *evalRecover {
		return nil
	}
	if ctx.Var == nil {
		return Int64ToInt8Slice(evalError(node.Tok, "not a compile-time constant"))
	}
	if ofs < 0 || (ctx.Var.Ty.Size < (ofs + node.Ty.Size)) {
		return Int64ToInt8Slice(evalError(node.Tok, "constexpr access out of bounds"))
	}
	if ctx.Var.ConstExprData != nil {
		return ctx.Var.ConstExprData[ofs:]
	}
	if ctx.Var.InitData != nil {
		return ctx.Var.InitData[ofs:]
	}
	panic("internal error")
}

func constExpr(rest **Token, tok *Token) int64 {
	node := conditional(rest, tok)
	node.addType()
	if !node.Ty.isInteger() {
		errorTok(tok, "constant expression not integer")
	}
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

	// If A is an atomic type, Convert `A op= B` to atomic_op_fetch(&A, B)
	if binary.Lhs.Ty.IsAtomic {
		return atomicOp(binary, false)
	}

	// Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
	if binary.Lhs.isBitField() {
		v := newLocalVar("", pointerTo(binary.Lhs.Lhs.Ty))

		expr1 := newBinary(ND_ASSIGN, newVarNode(v, tok), newUnary(ND_ADDR, binary.Lhs.Lhs, tok), tok)
		expr2 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(v, tok), tok), tok)
		expr2.Member = binary.Lhs.Member
		expr3 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(v, tok), tok), tok)
		expr3.Member = binary.Lhs.Member
		expr4 := newBinary(ND_ASSIGN, expr2, newBinary(binary.Kind, expr3, binary.Rhs, tok), tok)
		return newBinary(ND_CHAIN, expr1, expr4, tok)
	}

	// Convert `A op= B` to ``tmp = &A, *tmp = *tmp op B`.
	variable := newLocalVar("", pointerTo(binary.Lhs.Ty))

	expr1 := newBinary(ND_ASSIGN, newVarNode(variable, tok), newUnary(ND_ADDR, binary.Lhs, tok), tok)

	expr2 := newBinary(ND_ASSIGN, newUnary(ND_DEREF, newVarNode(variable, tok), tok), newBinary(binary.Kind, newUnary(ND_DEREF, newVarNode(variable, tok), tok), binary.Rhs, tok), tok)

	return newBinary(ND_CHAIN, expr1, expr2, tok)
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

// asm-stmt = "__asm__" ("volatile" | "inline")* "(" string-literal ")"
func asmStmt(rest **Token, tok *Token) *AstNode {
	node := newNode(ND_ASM, tok)
	tok = tok.Next

	for tok.isEqual("volatile") || tok.isEqual("inline") {
		tok = tok.Next
	}

	tok = skip(tok, "(")
	if tok.Kind != TK_STR || tok.Ty.Base.Kind != TY_PCHAR {
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
		enterTmpScope()
		v := newLocalVar("", cond.Ty)
		lhs := newBinary(ND_ASSIGN, newVarNode(v, tok), cond, tok)
		rhs := newNode(ND_COND, tok)
		rhs.Cond = newVarNode(v, tok).toBool()
		rhs.Then = newVarNode(v, tok)
		rhs.Else = conditional(rest, tok.Next.Next)
		leaveScope()
		return newBinary(ND_CHAIN, lhs, rhs, tok)
	}

	node := newNode(ND_COND, tok)
	node.Cond = cond.toBool()
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
	node.addType()

	// Convert A = B to (tmp = B, atomic_exchange(&A, tmp), tmp)
	if tok.isEqual("=") && node.Ty.IsAtomic {
		rhs := assign(rest, tok.Next)
		rhs.addType()
		tmp := newLocalVar("", rhs.Ty)
		expr := newBinary(ND_ASSIGN, newVarNode(tmp, tok), rhs, tok)
		chainExpr(&expr, newBinary(ND_EXCH, newUnary(ND_ADDR, node, tok), newVarNode(tmp, tok), tok))
		chainExpr(&expr, newVarNode(tmp, tok))
		return expr
	}

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
		if node.Ty.IsUnsigned {
			return toAssign(newBinary(ND_SHR, node, assign(rest, tok.Next), tok))
		} else {
			return toAssign(newBinary(ND_SAR, node, assign(rest, tok.Next), tok))
		}
	}

	*rest = tok
	return node
}

// logor = logand ("||" logand)*
func logor(rest **Token, tok *Token) *AstNode {
	node := logand(&tok, tok)
	for tok.isEqual("||") {
		start := tok
		node = newBinary(ND_LOGOR, node.toBool(), logand(&tok, tok.Next).toBool(), start)
	}
	*rest = tok
	return node
}

// logand = bitor ("&&" bitor)*
func logand(rest **Token, tok *Token) *AstNode {
	node := bitor(&tok, tok)
	for tok.isEqual("&&") {
		start := tok
		node = newBinary(ND_LOGAND, node.toBool(), bitor(&tok, tok.Next).toBool(), start)
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
			node = newBinary(ND_GT, node, shift(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual(">=") {
			node = newBinary(ND_GE, node, shift(&tok, tok.Next), start)
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
			node.addType()
			if node.Ty.IsUnsigned {
				node = newBinary(ND_SHR, node, add(&tok, tok.Next), start)
			} else {
				node = newBinary(ND_SAR, node, add(&tok, tok.Next), start)
			}
			continue
		}

		*rest = tok
		return node
	}
}

func skipParen(tok *Token) *Token {
	level := 0
	start := tok
	for {
		if level == 0 && tok.isEqual(")") {
			break
		}

		if tok.Kind == TK_EOF {
			errorTok(start, "unterminated list")
		}

		if tok.isEqual("(") {
			level++
		} else if tok.isEqual(")") {
			level--
		}

		tok = tok.Next
	}
	return tok.Next
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

	// VLA + num
	if lhs.Ty.Base.Kind == TY_VLA {
		rhs = newBinary(ND_MUL, rhs, newVarNode(lhs.Ty.Base.VlaSize, tok), tok)
		return newBinary(ND_ADD, lhs, rhs, tok)
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

	// VLA - num
	if lhs.Ty.Base.Kind == TY_VLA {
		rhs = newBinary(ND_MUL, rhs, newVarNode(lhs.Ty.Base.VlaSize, tok), tok)
		return newBinary(ND_SUB, lhs, rhs, tok)
	}

	// ptr - num
	if lhs.Ty.Base != nil && rhs.Ty.isInteger() {
		rhs = newBinary(ND_MUL, rhs, newLong(lhs.Ty.Base.Size, tok), tok)
		return newBinary(ND_SUB, lhs, rhs, tok)
	}

	// ptr - ptr, which returns how many elements are between the two.
	if lhs.Ty.Base != nil && rhs.Ty.Base != nil {
		sz := lhs.Ty.Base.Size
		node := newBinary(ND_SUB, newCast(lhs, TyLLong), newCast(rhs, TyLLong), tok)
		return newBinary(ND_DIV, node, newNum(sz, tok), tok)
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
 *       | "&&" ident
 *       | postfix
 */
func unary(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("+") {
		return newUnary(ND_POS, castExpr(rest, tok.Next), tok)
	}

	if tok.isEqual("-") {
		return newUnary(ND_NEG, castExpr(rest, tok.Next), tok)
	}

	if tok.isEqual("&") {
		lhs := castExpr(rest, tok.Next)
		lhs.addType()
		if lhs.isBitField() {
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

		ty := node.Ty
		node = newUnary(ND_DEREF, node, tok)
		if ty.isArray() {
			applyCvQualifier(node, ty)
		}
		return node
	}

	if tok.isEqual("!") {
		return newUnary(ND_NOT, castExpr(rest, tok.Next).toBool(), tok)
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

	// [GNU] labels-as-values
	if tok.isEqual("&&") {
		node := newNode(ND_LABEL_VAL, tok)
		node.Label = tok.Next.getIdent()
		node.GotoNext = gotos
		gotos = node
		DontDeallocVLA = true
		*rest = tok.Next.Next
		return node
	}

	return postfix(rest, tok)
}

// Convert A++ to `(ptr = &A, tmp = *ptr, *ptr += 1, tmp)`
func newIncDec(node *AstNode, tok *Token, addend int) *AstNode {
	node.addType()
	enterTmpScope()
	if node.isBitField() {
		tmp := newLocalVar("", node.Ty)
		ptr := newLocalVar("", pointerTo(node.Lhs.Ty))

		expr := newBinary(ND_ASSIGN, newVarNode(ptr, tok), newUnary(ND_ADDR, node.Lhs, tok), tok)

		memref1 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(ptr, tok), tok), tok)

		memref1.Member = node.Member

		memref2 := newUnary(ND_MEMBER, newUnary(ND_DEREF, newVarNode(ptr, tok), tok), tok)

		memref2.Member = node.Member

		chainExpr(&expr, newBinary(ND_ASSIGN, newVarNode(tmp, tok), memref1, tok))
		chainExpr(&expr, toAssign(newAdd(memref2, newNum(int64(addend), tok), tok)))
		chainExpr(&expr, newVarNode(tmp, tok))
		leaveScope()
		return expr
	}

	tmp := newLocalVar("", node.Ty)
	ptr := newLocalVar("", pointerTo(node.Ty))

	expr := newBinary(ND_ASSIGN, newVarNode(ptr, tok), newUnary(ND_ADDR, node, tok), tok)
	chainExpr(&expr, newBinary(ND_ASSIGN, newVarNode(tmp, tok), newUnary(ND_DEREF, newVarNode(ptr, tok), tok), tok))
	chainExpr(&expr, toAssign(newAdd(newUnary(ND_DEREF, newVarNode(ptr, tok), tok), newNum(int64(addend), tok), tok)))
	chainExpr(&expr, newVarNode(tmp, tok))
	leaveScope()
	return expr
}

/*
 * postfix = ident "(" func-args ")" postfix-tail*
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

			node.addType()
			ty := node.Ty
			node = newUnary(ND_DEREF, newAdd(node, idx, start), start)

			if ty.isArray() {
				applyCvQualifier(node, ty)
			}
			continue
		}

		if tok.isEqual(".") {
			node = structRef(node, tok.Next)
			tok = tok.Next.Next
			continue
		}

		if tok.isEqual("->") {
			// x->y is short for (*x).y
			node.addType()
			ty := node.Ty
			node = structRef(newUnary(ND_DEREF, node, tok), tok.Next)

			if ty.isArray() {
				applyCvQualifier(node, ty)
			}
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
	param := ty.ParamList
	if ty.IsOldStyle {
		param = nil
	}

	head := Obj{}
	cur := &head
	var expr *AstNode = nil

	enterTmpScope()

	for commaList(rest, &tok, ")", cur != &head) {
		arg := assign(&tok, tok)
		arg.addType()

		if param != nil {
			if param.Ty.Kind != TY_STRUCT && param.Ty.Kind != TY_UNION {
				arg = newCast(arg, param.Ty)
			}
			param = param.ParamNext
		} else {
			if !ty.IsVariadic && !ty.IsOldStyle {
				errorTok(tok, "too many arguments")
			}

			if arg.Ty.Kind == TY_FLOAT {
				arg = newCast(arg, TyDouble)
			} else if arg.Ty.isArray() {
				arg = newCast(arg, pointerTo(arg.Ty.Base))
			} else if arg.Ty.Kind == TY_FUNC {
				arg = newCast(arg, pointerTo(arg.Ty))
			}
		}

		arg.addType()

		v := newLocalVar("", arg.Ty)
		chainExpr(&expr, newBinary(ND_ASSIGN, newVarNode(v, tok), arg, tok))
		expr.addType()

		cur.ParamNext = v
		cur = cur.ParamNext
	}

	if param != nil {
		errorTok(tok, "too few arguments")
	}

	leaveScope()

	node := newUnary(ND_FUNCALL, fn, tok)
	node.Ty = ty.ReturnType
	node.Args = head.ParamNext
	node.ArgsExpr = expr

	// If a function returns a struct, it is caller's responsibility
	// to allocate a space for the return value.
	if node.Ty.Kind == TY_STRUCT || node.Ty.Kind == TY_UNION {
		node.ReturnBuffer = newLocalVar("", node.Ty)
	}

	return node
}

func isStringToken(rest **Token, tok *Token, strToken **Token) bool {
	if tok.isEqual("(") && isStringToken(&tok, tok.Next, strToken) && consume(rest, tok, ")") {
		return true
	}

	if tok.Kind == TK_STR {
		*strToken = tok
		*rest = tok.Next
		return true
	}
	return false
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
	} else if t1.Kind == TY_ARRAY || t1.Kind == TY_VLA {
		t1 = pointerTo(t1.Base)
	}

	var ret *AstNode = nil

	for commaList(rest, &tok, ")", true) {
		if tok.isEqual("default") {
			tok = skip(tok.Next, ":")
			node := assign(&tok, tok)
			if ret == nil {
				ret = node
			}
			continue
		}

		t2 := typeName(&tok, tok)
		if t2.Kind == TY_FUNC {
			errorTok(tok, "association has function type")
		}
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

func findFunction(name string) *Obj {
	sc := scope
	for sc.Parent != nil {
		sc = sc.Parent
	}

	sc2 := sc.Vars[name]
	if sc2 != nil && sc2.Variable != nil && sc2.Variable.IsFunction {
		return sc2.Variable
	}

	return nil
}

func markLive(v *Obj) {
	if !v.IsFunction || v.IsLive {
		return
	}

	v.IsLive = true

	for i := range v.Refs {
		fn := findFunction(v.Refs[i])
		if fn != nil {
			markLive(fn)
		}
	}
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

	if tok.isEqual("(") && tok.Next.isTypename() {
		// Compound literal
		start := tok
		ty := typeName(&tok, tok.Next)
		if ty.Kind == TY_VLA {
			errorTok(tok, "compound literals cannot be VLA")
		}
		tok = skip(tok, ")")

		if scope.Parent == nil {
			v := newAnonymousGlobalVariable(ty)
			globalVarInitializer(rest, tok, v)
			return newVarNode(v, start)
		}

		sc := scope
		for sc.IsTemporary {
			sc = sc.Parent
		}
		v := newVar("", ty)
		v.IsLocal = true
		v.Next = sc.Locals
		sc.Locals = v

		lhs := localVarInitializer(rest, tok, v)
		rhs := newVarNode(v, tok)
		return newBinary(ND_CHAIN, lhs, rhs, start)
	}

	if tok.isEqual("(") && tok.Next.isEqual("{") {
		if scope.Parent == nil {
			errorTok(tok, "statement expression at file scope")
		}

		node := compoundStmt(&tok, tok.Next.Next, ND_STMT_EXPR)

		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("sizeof") {
		var ty *CType = nil
		if tok.Next.isEqual("(") && tok.Next.Next.isTypename() {
			ty = typeName(&tok, tok.Next.Next)
			*rest = skip(tok, ")")
		} else {
			node := unary(rest, tok.Next)
			node.addType()
			ty = node.Ty
		}

		if ty.Kind == TY_VLA {
			if ty.VlaSize != nil {
				return newVarNode(ty.VlaSize, tok)
			}

			return computeVlaSize(ty, tok)
		}
		if ty.Size < 0 {
			errorTok(tok, "sizeof applied to incomplete type")
		}
		if ty.Kind == TY_STRUCT && ty.IsFlexible {
			mem := ty.Members
			for mem.Next != nil {
				mem = mem.Next
			}
			if mem.Ty.Kind == TY_ARRAY {
				return newULong(ty.Size-mem.Ty.Size, start)
			}
		}
		return newULong(ty.Size, start)
	}

	if tok.isEqual("(") {
		node := expr(&tok, tok.Next)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("_Alignof") {
		tok = skip(tok.Next, "(")
		if !tok.isTypename() {
			errorTok(tok, "expected type name")
		}
		ty := typeName(&tok, tok)
		for ty.Kind == TY_VLA || ty.Kind == TY_ARRAY {
			ty = ty.Base
		}
		*rest = skip(tok, ")")
		return newULong(ty.Align, tok)
	}

	if tok.isEqual("_Generic") {
		return genericSelection(rest, tok.Next)
	}

	if tok.isEqual("__builtin_alloca") {
		node := newNode(ND_ALLOCA, tok)
		tok = skip(tok.Next, "(")
		node.Lhs = assign(&tok, tok)
		*rest = skip(tok, ")")
		node.Ty = pointerTo(TyVoid)
		return node
	}

	text := tok.getText()
	if strings.HasPrefix(text, "__builtin_atomic_fetch_") {
		start := tok
		tok = skip(tok.Next, "(")
		obj := newUnary(ND_DEREF, assign(&tok, tok), start)
		tok = skip(tok, ",")
		val := assign(&tok, tok)
		*rest = skip(tok, ")")

		var binary *AstNode

		if strings.HasPrefix(text, "__builtin_atomic_fetch_add") {
			binary = newAdd(obj, val, start)
		} else if strings.HasPrefix(text, "__builtin_atomic_fetch_sub") {
			binary = newSub(obj, val, start)
		} else if strings.HasPrefix(text, "__builtin_atomic_fetch_and") {
			binary = newBinary(ND_BITAND, obj, val, start)
		} else if strings.HasPrefix(text, "__builtin_atomic_fetch_or") {
			binary = newBinary(ND_BITOR, obj, val, start)
		} else if strings.HasPrefix(text, "__builtin_atomic_fetch_xor") {
			binary = newBinary(ND_BITXOR, obj, val, start)
		} else {
			errorTok(start, "unsupported atomic fetch op")
		}

		binary.Lhs.addType()
		binary.Rhs.addType()
		return atomicOp(binary, true)
	}

	if tok.isEqual("__builtin_constant_p") {
		node := newNode(ND_NUM, tok)
		tok = skip(tok.Next, "(")
		node.Value = int64(boolToInt(expr(&tok, tok).isConstExpr(nil)))
		node.Ty = TyInt
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_expect") {
		tok = skip(tok.Next, "(")
		node := assign(&tok, tok)
		tok = skip(tok, ",")
		assign(&tok, tok)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_offsetof") {
		tok = skip(tok.Next, "(")
		ty := typeName(&tok, tok)
		tok = skip(tok, ",")

		var node *AstNode = nil
		offset := 0

		for {
			var mem *Member
			for {
				mem = structDesignator(&tok, tok, ty)
				offset += int(mem.Offset)
				ty = mem.Ty
				if mem.Name != nil {
					break
				}
			}

			for ; ty.Base != nil && consume(&tok, tok, "["); tok = skip(tok, "]") {
				ty = ty.Base
				expr := conditional(&tok, tok)
				val := int64(0)
				if expr.isConstExpr(&val) {
					offset += int(ty.Size * val)
					continue
				}
				if node == nil {
					node = newBinary(ND_MUL, expr, newLong(ty.Size, tok), tok)
				} else {
					node = newBinary(ND_ADD, node, newBinary(ND_MUL, expr, newLong(ty.Size, tok), tok), tok)
				}
			}

			if !consume(&tok, tok, ".") {
				break
			}
		}

		*rest = skip(tok, ")")
		if node == nil {
			return newULong(int64(offset), tok)
		}
		return newBinary(ND_ADD, node, newULong(int64(offset), tok), tok)
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

	if tok.isEqual("__builtin_unreachable") {
		node := newNode(ND_NULL_EXPR, tok)
		node.Ty = TyVoid
		*rest = skip(skip(tok.Next, "("), ")")
		return node
	}

	if tok.isEqual("__builtin_va_start") {
		node := newNode(ND_VA_START, tok)
		tok = skip(tok.Next, "(")
		node.Lhs = conditional(&tok, tok)
		if tok.isEqual(",") {
			assign(&tok, tok.Next)
		}
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_va_copy") {
		node := newNode(ND_VA_COPY, tok)
		tok = skip(tok.Next, "(")
		node.Lhs = conditional(&tok, tok)
		tok = skip(tok, ",")
		node.Rhs = conditional(&tok, tok)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_va_end") {
		tok = skip(tok.Next, "(")
		node := conditional(&tok, tok)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_va_arg") {
		node := newNode(ND_VA_ARG, tok)
		tok = skip(tok.Next, "(")

		apArg := conditional(&tok, tok)
		apArg.addType()
		node.Lhs = apArg
		tok = skip(tok, ",")

		ty := typeName(&tok, tok)
		if ty.vaArgNeedCopy() {
			node.Variable = newLocalVar("", ty)
		}

		node.Ty = pointerTo(ty)
		*rest = skip(tok, ")")
		return newUnary(ND_DEREF, node, tok)
	}

	if tok.isEqual("__builtin_atomic_exchange") || tok.isEqual("__sync_lock_test_and_set") {
		node := newNode(ND_EXCH, tok)
		tok = skip(tok.Next, "(")
		node.Lhs = assign(&tok, tok)
		tok = skip(tok, ",")
		node.Rhs = assign(&tok, tok)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__sync_lock_release") {
		node := newNode(ND_LOCK_RELEASE, tok) // 自定义节点类型
		tok = skip(tok.Next, "(")
		node.Lhs = assign(&tok, tok) // 解析参数 ptr
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("__builtin_compare_and_swap") {
		node := newNode(ND_CAS, tok)
		tok = skip(tok.Next, "(")
		node.CasAddr = assign(&tok, tok)
		tok = skip(tok, ",")
		node.CasOld = assign(&tok, tok)
		tok = skip(tok, ",")
		node.CasNew = assign(&tok, tok)
		*rest = skip(tok, ")")
		return node
	}

	if tok.Kind == TK_IDENT {
		// Variable or enum constant
		sc := findVariable(tok)
		*rest = tok.Next

		// For "static inline" function
		if sc != nil && sc.Variable != nil && sc.Variable.IsFunction {
			if currentFunction != nil {
				currentFunction.Refs = append(currentFunction.Refs, sc.Variable.Name)
			} else {
				sc.Variable.IsRoot = true
			}

			name := sc.Variable.Name
			if name == "alloca" {
				DontDeallocVLA = true
			}
			if strings.Contains(name, "setjmp") || strings.Contains(name, "savectx") || strings.Contains(name, "vfork") || strings.Contains(name, "getcontext") {
				DontReuseStack = true
			}
		}

		if sc != nil {
			if sc.Variable != nil {
				return newVarNode(sc.Variable, tok)
			} else {
				return newNum(int64(sc.EnumValue), tok)
			}
		}

		// [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
		// automatically defined as a local variable containing the
		// current function name.
		// [GNU] __FUNCTION__ is yet another name of __func__.
		if currentFunction != nil && (tok.isEqual("__func__") || tok.isEqual("__FUNCTION__")) {
			name := currentFunction.Name
			vsc := &VarScope{}
			buf := U82I8([]uint8(name))
			buf = append(buf, 0)
			vsc.Variable = newStringLiteral(buf, arrayOf(TyPChar, int64(len(buf))))
			if currentFunction.Ty.Scopes.Vars == nil {
				currentFunction.Ty.Scopes.Vars = map[string]*VarScope{}
			}
			currentFunction.Ty.Scopes.Vars["__func__"] = vsc
			currentFunction.Ty.Scopes.Vars["__FUNCTION__"] = vsc
			return newVarNode(vsc.Variable, tok)
		}

		if tok.Next.isEqual("(") {
			errorTok(tok, "implicit declaration of a function")
		}
		errorTok(tok, "undefined variable")
	}

	if tok.Kind == TK_STR {
		v := newStringLiteral(tok.StringLiteral, tok.Ty)
		*rest = tok.Next
		n := newVarNode(v, tok)
		n.addType()
		return n
	}

	if tok.Kind == TK_PP_NUM {
		convertPpNumber(tok)
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

func parseTypeDef(rest **Token, tok *Token, basety *CType, attr *VarAttr) *AstNode {
	first := true
	var node *AstNode = nil

	for ; commaList(rest, &tok, ";", !first); first = false {
		var name *Token = nil
		altAlign := attr.Align
		ty := declarator2(&tok, tok, basety, &name, &altAlign)
		if name == nil {
			errorTok(name, "typedef name omitted")
		}
		if altAlign != 0 {
			ty = newQualifiedType(ty)
			ty.Align = altAlign
		}
		pushScope(name.getIdent()).TypeDef = ty
		chainExpr(&node, computeVlaSize(ty, tok))
	}

	return node
}

func funcPrototype(ty *CType, attr *VarAttr, name *Token) *Obj {
	nameString := name.getIdent()

	fn := findFunction(nameString)
	if fn == nil {
		fn = newGlobalVar(nameString, ty)
		fn.IsFunction = true
		fn.IsStatic = attr.IsStatic || (attr.IsInline && !attr.IsExtern)
		fn.IsInline = attr.IsInline
	} else if !fn.IsStatic && attr.IsStatic {
		errorTok(name, "static declaration follows a non-static declaration")
	}

	fn.IsRoot = !(fn.IsStatic && fn.IsInline)
	return fn
}

func funcDefinition(rest **Token, tok *Token, ty *CType, attr *VarAttr, name *Token) {
	fn := funcPrototype(ty, attr, name)

	if fn.IsDefinition {
		errorTok(tok, "redefinition of "+fn.Name)
	}
	fn.IsDefinition = true
	fn.Ty = ty

	currentFunction = fn
	CurrentVLA = nil
	FnUseVLA = false
	DontDeallocVLA = false

	if ty.Scopes != nil {
		scope = ty.Scopes
	} else {
		enterScope()
		ty.Scopes = scope
	}

	fn.Body = compoundStmt(rest, tok.Next, ND_BLOCK)
	if ty.PreCalc != nil {
		calc := newUnary(ND_EXPR_STMT, ty.PreCalc, tok)
		calc.Next = fn.Body.Body
		fn.Body.Body = calc
	}

	if FnUseVLA && !DontDeallocVLA && !DontReuseStack {
		fn.DeallocVLA = true
	}

	leaveScope()
	resolveGotoLabels()
	currentFunction = nil
}

func globalDeclaration(tok *Token, basety *CType, attr *VarAttr) *Token {
	first := true

	for ; commaList(&tok, &tok, ";", !first); first = false {
		var name *Token = nil
		altAlign := attr.Align
		ty := declarator2(&tok, tok, basety, &name, &altAlign)
		if ty.Kind == TY_FUNC {
			if tok.isEqual("{") {
				if !first || scope.Parent != nil {
					errorTok(tok, "function definition is not allowed here")
				}
				funcDefinition(&tok, tok, ty, attr, name)
				return tok
			}
			funcPrototype(ty, attr, name)
			continue
		}

		if name == nil {
			errorTok(tok, "variable name omitted")
		}

		variable := newGlobalVar(name.getIdent(), ty)
		variable.IsDefinition = !attr.IsExtern
		variable.IsStatic = attr.IsStatic
		variable.IsTls = attr.IsTls
		if altAlign > 0 {
			variable.Align = altAlign
		}

		if attr.IsConstExpr {
			if !tok.isEqual("=") {
				errorTok(tok, "constexpr variable not initialized")
			}
			constExprInitializer(&tok, tok.Next, variable, variable)
			variable.IsStatic = true
			continue
		}

		if tok.isEqual("=") {
			globalVarInitializer(&tok, tok.Next, variable)
		} else if !attr.IsExtern && !attr.IsTls {
			variable.IsTentative = true
		}
	}

	return tok
}

// Remove redundant tentative definitions.
func scanGlobals() {
	head := Obj{}
	cur := &head

	for v := globals; v != nil; v = v.Next {
		if !v.IsTentative {
			cur.Next = v
			cur = cur.Next
			continue
		}

		// Find another definition of the same identifier.
		v2 := globals
		for ; v2 != nil; v2 = v2.Next {
			if v != v2 && v2.IsDefinition && v.Name == v2.Name {
				break
			}
		}

		// If there's another definition, the tentative definition
		// is redundant
		if v2 == nil {
			cur.Next = v
			cur = cur.Next
		}
	}

	cur.Next = nil
	globals = head.Next
}

// program = (function-definition | global-variable)*
func parse(tok *Token) *Obj {
	globals = nil

	for tok.Kind != TK_EOF {
		if tok.isEqual("_Static_assert") {
			staticAssertion(&tok, tok.Next)
			continue
		}

		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)

		// Typedef
		if attr.IsTypeDef {
			parseTypeDef(&tok, tok, basety, &attr)
			continue
		}

		tok = globalDeclaration(tok, basety, &attr)
	}

	for v := globals; v != nil; v = v.Next {
		if v.IsRoot {
			markLive(v)
		}
	}

	// Remove redundant tentative definitions.
	scanGlobals()
	return globals
}
