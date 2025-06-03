package main

type CTypeKind uint8

const (
	TY_VOID CTypeKind = iota
	TY_BOOL
	TY_CHAR
	TY_INT
	TY_SHORT
	TY_LONG
	TY_ENUM
	TY_PTR
	TY_FUNC
	TY_ARRAY
	TY_STRUCT
	TY_UNION
)

type CType struct {
	Kind  CTypeKind
	Size  int64 // sizeof() value
	Align int64 // alignment

	Base *CType

	Name *Token

	ArrayLength int64

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

var TyVoid = &CType{
	Kind:  TY_VOID,
	Size:  1,
	Align: 1,
}

var TyBool = &CType{
	Kind:  TY_BOOL,
	Size:  1,
	Align: 1,
}

var TyChar = &CType{
	Kind:  TY_CHAR,
	Size:  1,
	Align: 1,
}

var TyShort = &CType{
	Kind:  TY_SHORT,
	Size:  2,
	Align: 2,
}

var TyInt = &CType{
	Kind:  TY_INT,
	Size:  4,
	Align: 4,
}

var TyLong = &CType{
	Kind:  TY_LONG,
	Size:  8,
	Align: 8,
}

func newType(kind CTypeKind, size int64, align int64) *CType {
	ty := &CType{
		Kind:  kind,
		Size:  size,
		Align: align,
	}

	return ty
}

func (t *CType) isInteger() bool {
	return t.Kind == TY_CHAR || t.Kind == TY_INT || t.Kind == TY_LONG || t.Kind == TY_SHORT || t.Kind == TY_BOOL || t.Kind == TY_ENUM
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

func arrayOf(base *CType, len int64) *CType {
	ty := newType(TY_ARRAY, base.Size*len, base.Align)
	ty.Base = base
	ty.ArrayLength = len
	return ty
}

func enumType() *CType {
	return newType(TY_ENUM, 4, 4)
}

func structType() *CType {
	return newType(TY_STRUCT, 0, 1)
}

func (ty *CType) getCommonType(other *CType) *CType {
	if ty.Base != nil {
		return pointerTo(ty.Base)
	}

	if ty.Size == 8 || other.Size == 8 {
		return TyLong
	}

	return TyInt
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
func usualArithConv(lhs **AstNode, rhs **AstNode) {
	ty := (*lhs).Ty.getCommonType((*rhs).Ty)
	*lhs = newCast(*lhs, ty)
	*rhs = newCast(*rhs, ty)
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
	case ND_NUM:
		if node.Value == int64(int(node.Value)) {
			node.Ty = TyInt
		} else {
			node.Ty = TyLong
		}
		return
	case ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_MOD, ND_BITAND, ND_BITOR, ND_BITXOR:
		usualArithConv(&node.Lhs, &node.Rhs)
		node.Ty = node.Lhs.Ty
		return
	case ND_NEG:
		ty := TyInt.getCommonType(node.Lhs.Ty)
		node.Lhs = newCast(node.Lhs, ty)
		node.Ty = ty
		return
	case ND_ASSIGN:
		if node.Lhs.Ty.Kind == TY_ARRAY {
			errorTok(node.Lhs.Tok, "not an lvalue")
		}
		if node.Lhs.Ty.Kind != TY_STRUCT {
			node.Rhs = newCast(node.Rhs, node.Lhs.Ty)
		}
		node.Ty = node.Lhs.Ty
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		usualArithConv(&node.Lhs, &node.Rhs)
		node.Ty = TyInt
		return
	case ND_FUNCALL:
		node.Ty = TyLong
		return
	case ND_NOT, ND_LOGAND, ND_LOGOR:
		node.Ty = TyInt
		return
	case ND_BITNOT, ND_SHL, ND_SHR:
		node.Ty = node.Lhs.Ty
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
		if node.Lhs.Ty.Base.Kind == TY_VOID {
			errorTok(node.Tok, "dereferencing a void pointer")
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
