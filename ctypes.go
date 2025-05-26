package main

type CTypeKind uint8

const (
	TY_CHAR CTypeKind = iota
	TY_INT
	TY_PTR
	TY_FUNC
	TY_ARRAY
	TY_STRUCT
)

type CType struct {
	Kind  CTypeKind
	Size  int // sizeof() value
	Align int // alignment

	Base *CType

	Name *Token

	ArrayLength int

	// Struct
	Members *Member

	ReturnType *CType
	Params     *CType
	Next       *CType
}

func (ty *CType) copy() *CType {
	ret := &CType{}
	*ret = *ty
	return ret
}

var TyChar = &CType{
	Kind:  TY_CHAR,
	Size:  1,
	Align: 1,
}

var TyInt = &CType{
	Kind:  TY_INT,
	Size:  8,
	Align: 8,
}

func newType(kind CTypeKind, size int, align int) *CType {
	ty := &CType{
		Kind:  kind,
		Size:  size,
		Align: align,
	}

	return ty
}

func (t *CType) isInteger() bool {
	return t.Kind == TY_CHAR || t.Kind == TY_INT
}

func pointerTo(base *CType) *CType {
	ty := newType(TY_PTR, 8, 8) // Assuming pointer size is 8 bytes
	ty.Base = base
	return ty
}

func funcType(returnTy *CType) *CType {
	ty := &CType{
		Kind: TY_FUNC,
	}
	ty.ReturnType = returnTy
	return ty
}

func arrayOf(base *CType, len int) *CType {
	ty := newType(TY_ARRAY, base.Size*len, base.Align)
	ty.Base = base
	ty.ArrayLength = len
	return ty
}

func (node *AstNode) addType() {
	if node == nil || node.Ty != nil {
		return
	}

	node.Lhs.addType()
	node.Rhs.addType()
	node.Cond.addType()
	node.Then.addType()
	node.Else.addType()
	node.Init.addType()
	node.Inc.addType()

	for n := node.Body; n != nil; n = n.Next {
		n.addType()
	}
	for n := node.Args; n != nil; n = n.Next {
		n.addType()
	}

	switch node.Kind {
	case ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_NEG:
		node.Ty = node.Lhs.Ty
		return
	case ND_ASSIGN:
		if node.Lhs.Ty.Kind == TY_ARRAY {
			errorTok(node.Lhs.Tok, "not an lvalue")
		}
		node.Ty = node.Lhs.Ty
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE, ND_NUM, ND_FUNCALL:
		node.Ty = TyInt
		return
	case ND_VAR:
		node.Ty = node.Variable.Ty
		return
	case ND_COMMA:
		node.Ty = node.Rhs.Ty
		return
	case ND_MEMBER:
		node.Ty = node.Member.Ty
		return
	case ND_ADDR:
		if node.Lhs.Ty.Kind == TY_ARRAY {
			node.Ty = pointerTo(node.Lhs.Ty.Base)
		} else {
			node.Ty = pointerTo(node.Lhs.Ty)
		}
		return
	case ND_DEREF:
		if node.Lhs.Ty.Base == nil {
			errorTok(node.Tok, "invalid pointer dereference")
		}
		node.Ty = node.Lhs.Ty.Base
		return
	case ND_STMT_EXPR:
		if node.Body != nil {
			stmt := node.Body
			for stmt.Next != nil {
				stmt = stmt.Next
			}
			if stmt.Kind == ND_EXPR_STMT {
				node.Ty = stmt.Lhs.Ty
				return
			}
		}
		errorTok(node.Tok, "statement expression returning void is not supported")
		return
	}
}
