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

import "fmt"

// Scope for local or global variables.
type VarScope struct {
	Next     *VarScope
	Name     string
	Variable *Obj
}

// Scope for struct or union tags.
type TagScope struct {
	Next *TagScope
	Name string // Struct tag name
	Ty   *CType
}

// Represents a block scope.
type Scope struct {
	Next *Scope
	// C has two block scopes; one is for variables and the other is
	// for struct tags.
	Variables *VarScope
	Tags      *TagScope
}

var uniqueNameId int = 0

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Likewise, global variables are accumulated to this list.
var globals *Obj

var scope *Scope = &Scope{}

func enterScope() {
	sc := &Scope{}
	sc.Next = scope
	scope = sc
}

func leaveScope() {
	scope = scope.Next
}

// Find a variable by name.
func findVariable(tok *Token) *Obj {
	for sc := scope; sc != nil; sc = sc.Next {
		for sc2 := sc.Variables; sc2 != nil; sc2 = sc2.Next {
			if tok.isEqual(sc2.Name) {
				return sc2.Variable
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

func pushScope(name string, variable *Obj) *VarScope {
	sc := &VarScope{}
	sc.Name = name
	sc.Variable = variable
	sc.Next = scope.Variables
	scope.Variables = sc
	return sc
}

func pushTagScope(tok *Token, ty *CType) *TagScope {
	sc := &TagScope{}
	sc.Name = string((*currentInput)[tok.Location : tok.Location+tok.Length])
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

func newVarNode(variable *Obj, tok *Token) *AstNode {
	node := newNode(ND_VAR, tok)
	node.Variable = variable
	return node
}

func newVar(name string, ty *CType) *Obj {
	variable := &Obj{}
	variable.Name = name
	variable.Ty = ty
	pushScope(name, variable)
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

func newStringLiteral(lit []uint8, ty *CType) *Obj {
	variable := newAnonGlobalVar(ty)
	variable.InitData = lit
	return variable
}

func (tok *Token) getIdent() string {
	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected an identifier")
	}
	return string((*currentInput)[tok.Location : tok.Location+tok.Length])
}

func (tok *Token) getNumber() int64 {
	if tok.Kind != TK_NUM {
		errorTok(tok, "expected a number")
	}
	return tok.Value
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

// declspec = "char" | "short" | "int" | "long" | struct-decl | union-decl
func declspec(rest **Token, tok *Token) *CType {
	if tok.isEqual("char") {
		*rest = tok.Next
		return TyChar
	}

	if tok.isEqual("short") {
		*rest = tok.Next
		return TyShort
	}

	if tok.isEqual("int") {
		*rest = tok.Next
		return TyInt
	}

	if tok.isEqual("long") {
		*rest = tok.Next
		return TyLong
	}

	if tok.isEqual("struct") {
		return structDecl(rest, tok.Next)
	}

	if tok.isEqual("union") {
		return unionDecl(rest, tok.Next)
	}

	errorTok(tok, "expected a typename")
	return nil
}

// struct-members = (declspec declarator (","  declarator)* ";")*
func structMembers(rest **Token, tok *Token, ty *CType) {
	head := Member{}
	cur := &head

	for !tok.isEqual("}") {
		basety := declspec(&tok, tok)
		i := 0

		for !consume(&tok, tok, ";") {
			if i > 0 {
				tok = skip(tok, ",")
			}
			i += 1

			mem := &Member{}
			mem.Ty = declarator(&tok, tok, basety)
			mem.Name = mem.Ty.Name
			cur.Next = mem
			cur = cur.Next
		}
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
		ty := findTag(tag)
		if ty == nil {
			errorTok(tag, "unknown struct type")
		}
		*rest = tok
		return ty
	}

	// Construct a struct object.
	ty := &CType{
		Kind: TY_STRUCT,
	}
	structMembers(rest, tok.Next, ty)
	ty.Align = 1

	// Register the struct type if a name was given.
	if tag != nil {
		pushTagScope(tag, ty)
	}
	return ty
}

// struct-decl = struct-union-decl
func structDecl(rest **Token, tok *Token) *CType {
	ty := structUnionDecl(rest, tok)
	ty.Kind = TY_STRUCT

	// Assign offsets within the struct to members.
	var offset int64 = 0
	for mem := ty.Members; mem != nil; mem = mem.Next {
		offset = alignTo(offset, mem.Ty.Align)
		mem.Offset = offset
		offset += mem.Ty.Size

		if ty.Align < mem.Ty.Align {
			ty.Align = mem.Ty.Align
		}
	}
	ty.Size = alignTo(offset, ty.Align)
	return ty
}

// union-decl = struct-union-decl
func unionDecl(rest **Token, tok *Token) *CType {
	ty := structUnionDecl(rest, tok)
	ty.Kind = TY_UNION

	// If union, we don't have to assign offsets because they
	// are already initialized to zero. We need to compute the
	// alignment and the size though.
	for mem := ty.Members; mem != nil; mem = mem.Next {
		if ty.Align < mem.Ty.Align {
			ty.Align = mem.Ty.Align
		}

		if ty.Size < mem.Ty.Size {
			ty.Size = mem.Ty.Size
		}
	}
	ty.Size = alignTo(ty.Size, ty.Align)
	return ty
}

func getStructMember(ty *CType, tok *Token) *Member {
	for mem := ty.Members; mem != nil; mem = mem.Next {
		if mem.Name.isEqual(string((*currentInput)[tok.Location : tok.Location+tok.Length])) {
			return mem
		}
	}

	errorTok(tok, "unknown struct member")
	return nil
}

func structRef(lhs *AstNode, tok *Token) *AstNode {
	lhs.addType()
	if lhs.Ty.Kind != TY_STRUCT && lhs.Ty.Kind != TY_UNION {
		errorTok(lhs.Tok, "not a struct not a union")
	}

	node := newUnary(ND_MEMBER, lhs, tok)
	node.Member = getStructMember(lhs.Ty, tok)
	return node
}

// func-params = (param ("," param)*)? ")"
// param       = declspec declarator
func funcParams(rest **Token, tok *Token, ty *CType) *CType {
	head := CType{}
	cur := &head

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}

		basety := declspec(&tok, tok)
		ty := declarator(&tok, tok, basety)
		cur.Next = ty.copy()
		cur = cur.Next
	}

	ty = funcType(ty)
	ty.Params = head.Next
	*rest = tok.Next
	return ty
}

/*
 * type-suffix = "(" func-params
 *	           | "[" num "]" type-suffix
 *	           | ε
 */
func typeSuffix(rest **Token, tok *Token, ty *CType) *CType {
	if tok.isEqual("(") {
		return funcParams(rest, tok.Next, ty)
	}

	if tok.isEqual("[") {
		sz := tok.Next.getNumber()
		tok = skip(tok.Next.Next, "]")
		ty = typeSuffix(rest, tok, ty)
		return arrayOf(ty, sz)
	}

	*rest = tok
	return ty
}

// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(rest **Token, tok *Token, ty *CType) *CType {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)
	}

	if tok.isEqual("(") {
		start := tok
		dummy := CType{}
		declarator(&tok, tok.Next, &dummy)
		tok = skip(tok, ")")
		ty = typeSuffix(rest, tok, ty)
		return declarator(&tok, start.Next, ty)
	}

	if tok.Kind != TK_IDENT {
		errorTok(tok, "expected a variable name")
	}

	ty = typeSuffix(rest, tok.Next, ty)
	ty.Name = tok
	return ty
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token) *AstNode {
	basety := declspec(&tok, tok)

	head := AstNode{}
	cur := &head
	i := 0

	for !tok.isEqual(";") {
		if i > 0 {
			tok = skip(tok, ",")
		}

		i += 1

		ty := declarator(&tok, tok, basety)
		variable := newLocalVar(ty.Name.getIdent(), ty)

		if !tok.isEqual("=") {
			continue
		}

		lhs := newVarNode(variable, ty.Name)
		rhs := assign(&tok, tok.Next)
		node := newBinary(ND_ASSIGN, lhs, rhs, tok)
		cur.Next = newUnary(ND_EXPR_STMT, node, tok)
		cur = cur.Next
	}

	node := newNode(ND_BLOCK, tok)
	node.Body = head.Next
	*rest = tok.Next
	return node
}

// Returns true if a given token represents a type.
func (tok *Token) isTypename() bool {
	return tok.isEqual("char") || tok.isEqual("int") || tok.isEqual("struct") || tok.isEqual("union") || tok.isEqual("short") || tok.isEqual("long")
}

/*
 * stmt = "return" expr ";"
 *	    | "if" "(" expr ")" stmt ("else" stmt)?
 *	    | "for" "(" expr-stmt expr? ";" expr? ")" stmt
 *	    | "while" "(" expr ")" stmt
 *	    | "{" compound-stmt
 *	    | expr-stmt
 */
func stmt(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("return") {
		node := newNode(ND_RETURN, tok)
		node.Lhs = expr(&tok, tok.Next)
		*rest = skip(tok, ";")
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

	if tok.isEqual("for") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")

		node.Init = exprStmt(&tok, tok)

		if !tok.isEqual(";") {
			node.Cond = expr(&tok, tok)
		}
		tok = skip(tok, ";")

		if !tok.isEqual(")") {
			node.Inc = expr(&tok, tok)
		}
		tok = skip(tok, ")")

		node.Then = stmt(rest, tok)
		return node
	}

	if tok.isEqual("while") {
		node := newNode(ND_FOR, tok)
		tok = skip(tok.Next, "(")
		node.Cond = expr(&tok, tok)
		tok = skip(tok, ")")
		node.Then = stmt(rest, tok)
		return node
	}

	if tok.isEqual("{") {
		return compoundStmt(rest, tok.Next)
	}

	return exprStmt(rest, tok)
}

// compound-stmt = (declaration | stmt)* "}"
func compoundStmt(rest **Token, tok *Token) *AstNode {
	node := newNode(ND_BLOCK, tok)
	head := AstNode{}
	cur := &head

	enterScope()

	for !tok.isEqual("}") {
		if tok.isTypename() {
			cur.Next = declaration(&tok, tok)
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

// expr = assign ("," expr)?
func expr(rest **Token, tok *Token) *AstNode {
	node := assign(&tok, tok)
	if tok.isEqual(",") {
		return newBinary(ND_COMMA, node, expr(rest, tok.Next), tok)
	}

	*rest = tok
	return node
}

// assign = equality ("=" assign)?
func assign(rest **Token, tok *Token) *AstNode {
	node := equality(&tok, tok)

	if tok.isEqual("=") {
		return newBinary(ND_ASSIGN, node, assign(rest, tok.Next), tok)
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

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
func relational(rest **Token, tok *Token) *AstNode {
	node := add(&tok, tok)

	for {
		start := tok

		if tok.isEqual("<") {
			node = newBinary(ND_LT, node, add(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("<=") {
			node = newBinary(ND_LE, node, add(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual(">") {
			node = newBinary(ND_LT, add(&tok, tok.Next), node, start)
			continue
		}

		if tok.isEqual(">=") {
			node = newBinary(ND_LE, add(&tok, tok.Next), node, start)
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
	if lhs.Ty.isInteger() && rhs.Ty.isInteger() {
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
	rhs = newBinary(ND_MUL, rhs, newNum(lhs.Ty.Base.Size, tok), tok)
	return newBinary(ND_ADD, lhs, rhs, tok)
}

// Like `+`, `-` is overloaded for the pointer type.
func newSub(lhs *AstNode, rhs *AstNode, tok *Token) *AstNode {
	lhs.addType()
	rhs.addType()

	// num - num
	if lhs.Ty.isInteger() && rhs.Ty.isInteger() {
		return newBinary(ND_SUB, lhs, rhs, tok)
	}

	// ptr - num
	if lhs.Ty.Base != nil && rhs.Ty.isInteger() {
		rhs = newBinary(ND_MUL, rhs, newNum(lhs.Ty.Base.Size, tok), tok)
		rhs.addType()
		node := newBinary(ND_SUB, lhs, rhs, tok)
		node.Ty = lhs.Ty
		return node
	}

	// ptr - ptr, which returns how many elements are between the two.
	if lhs.Ty.Base != nil && rhs.Ty.Base != nil {
		node := newBinary(ND_SUB, lhs, rhs, tok)
		node.Ty = TyInt
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

// mul = unary ("*" unary | "/" unary)*
func mul(rest **Token, tok *Token) *AstNode {
	node := unary(&tok, tok)

	for {
		start := tok

		if tok.isEqual("*") {
			node = newBinary(ND_MUL, node, unary(&tok, tok.Next), start)
			continue
		}

		if tok.isEqual("/") {
			node = newBinary(ND_DIV, node, unary(&tok, tok.Next), start)
			continue
		}

		*rest = tok
		return node
	}
}

/*
 * unary = ("+" | "-" | "*" | "&") unary
 *       | postfix
 */
func unary(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("+") {
		return unary(rest, tok.Next)
	}

	if tok.isEqual("-") {
		return newUnary(ND_NEG, unary(rest, tok.Next), tok)
	}

	if tok.isEqual("&") {
		return newUnary(ND_ADDR, unary(rest, tok.Next), tok)
	}

	if tok.isEqual("*") {
		return newUnary(ND_DEREF, unary(rest, tok.Next), tok)
	}

	return postfix(rest, tok)
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
func postfix(rest **Token, tok *Token) *AstNode {
	node := primary(&tok, tok)

	for {
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

		*rest = tok
		return node
	}
}

// funcall = ident "(" (assign ("," assign)*)? ")"
func funcall(rest **Token, tok *Token) *AstNode {
	start := tok
	tok = tok.Next.Next

	head := AstNode{}
	cur := &head

	for !tok.isEqual(")") {
		if cur != &head {
			tok = skip(tok, ",")
		}
		cur.Next = assign(&tok, tok)
		cur = cur.Next
	}

	*rest = skip(tok, ")")

	node := newNode(ND_FUNCALL, start)
	node.FuncName = string((*currentInput)[start.Location : start.Location+start.Length])
	node.Args = head.Next
	return node
}

/*
 * primary = "(" "{" stmt+ "}" ")"
 *         | "(" expr ")"
 *         | "sizeof" unary
 *         | ident func-args?
 *         | str
 *         | num
 */
func primary(rest **Token, tok *Token) *AstNode {
	if tok.isEqual("(") && tok.Next.isEqual("{") {
		// This is a GNU statement expresssion.
		node := newNode(ND_STMT_EXPR, tok)
		node.Body = compoundStmt(&tok, tok.Next.Next).Body
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("(") {
		node := expr(&tok, tok.Next)
		*rest = skip(tok, ")")
		return node
	}

	if tok.isEqual("sizeof") {
		node := unary(rest, tok.Next)
		node.addType()
		return newNum(node.Ty.Size, tok)
	}

	if tok.Kind == TK_IDENT {
		// Function call
		if tok.Next.isEqual("(") {
			return funcall(rest, tok)
		}

		// Variable
		v := findVariable(tok)
		if v == nil {
			errorTok(tok, "undefined variable")
		}

		*rest = tok.Next
		return newVarNode(v, tok)
	}

	if tok.Kind == TK_STR {
		v := newStringLiteral([]uint8(tok.StringLiteral), tok.Ty)
		*rest = tok.Next
		return newVarNode(v, tok)
	}

	if tok.Kind == TK_NUM {
		node := newNum(tok.Value, tok)
		*rest = tok.Next
		return node
	}
	errorTok(tok, "expected an expression")
	return nil
}

func createParamLocalVars(param *CType) {
	if param != nil {
		createParamLocalVars(param.Next)
		newLocalVar(param.Name.getIdent(), param)
	}
}

func function(tok *Token, basety *CType) *Token {
	ty := declarator(&tok, tok, basety)

	fn := newGlobalVar(ty.Name.getIdent(), ty)
	fn.IsFunction = true

	locals = nil

	enterScope()
	createParamLocalVars(ty.Params)
	fn.Params = locals

	tok = skip(tok, "{")

	fn.Body = compoundStmt(&tok, tok)
	fn.Locals = locals
	leaveScope()
	return tok
}

func globalVariable(tok *Token, basety *CType) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = skip(tok, ",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		newGlobalVar(ty.Name.getIdent(), ty)
	}

	return tok
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
func isFunction(tok *Token) bool {
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
		basety := declspec(&tok, tok)

		// Function
		if isFunction(tok) {
			tok = function(tok, basety)
			continue
		}

		// Global variable
		tok = globalVariable(tok, basety)
	}

	return globals
}
