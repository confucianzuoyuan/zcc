package main

type CTypeKind uint8

const (
	TY_VOID CTypeKind = iota
	TY_BOOL
	TY_CHAR
	TY_INT
	TY_SHORT
	TY_LONG
	TY_FLOAT
	TY_DOUBLE
	TY_LDOUBLE
	TY_ENUM
	TY_PTR
	TY_FUNC
	TY_ARRAY
	TY_VLA // variable-length array
	TY_STRUCT
	TY_UNION
)

type CType struct {
	Kind       CTypeKind
	Size       int64  // sizeof() value
	Align      int64  // alignment
	IsUnsigned bool   // unsigned or signed
	IsAtomic   bool   // true if _Atomic
	Origin     *CType // for type compatibility check

	Base *CType

	// Declaration
	Name    *Token
	NamePos *Token

	ArrayLength int64

	// Variable-length array
	VlaLen  *AstNode
	VlaSize *Obj

	// Struct
	Members    *Member
	IsFlexible bool
	IsPacked   bool

	// Function type
	Scopes     *Scope
	ReturnType *CType
	ParamList  *Obj
	VlaCalc    *AstNode
	IsVariadic bool
}

func (t *CType) print() {
	switch t.Kind {
	case TY_VOID:
		println("void")
	case TY_ARRAY:
		println("array")
	case TY_BOOL:
		println("bool")
	case TY_CHAR:
		println("char")
	case TY_FLOAT:
		println("float")
	case TY_DOUBLE:
		println("double")
	case TY_PTR:
		print("ptr to ")
		t.Base.print()
	}
}

func (ty *CType) copy() *CType {
	ret := &CType{}
	*ret = *ty
	if ty.Kind == TY_STRUCT {
		head := Member{}
		cur := &head
		for mem := ty.Members; mem != nil; mem = mem.Next {
			m := &Member{}
			*m = *mem
			cur.Next = m
			cur = m
		}

		ret.Members = head.Next
		ret.Origin = ty
		return ret
	}
	return ret
}

func vlaOf(base *CType, length *AstNode) *CType {
	ty := newType(TY_VLA, 8, 8)
	ty.Base = base
	ty.VlaLen = length
	return ty
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

var TyUChar = &CType{
	Kind:       TY_CHAR,
	Size:       1,
	Align:      1,
	IsUnsigned: true,
}

var TyUShort = &CType{
	Kind:       TY_SHORT,
	Size:       2,
	Align:      2,
	IsUnsigned: true,
}

var TyUInt = &CType{
	Kind:       TY_INT,
	Size:       4,
	Align:      4,
	IsUnsigned: true,
}

var TyULong = &CType{
	Kind:       TY_LONG,
	Size:       8,
	Align:      8,
	IsUnsigned: true,
}

var TyFloat = &CType{
	Kind:  TY_FLOAT,
	Size:  4,
	Align: 4,
}

var TyDouble = &CType{
	Kind:  TY_DOUBLE,
	Size:  8,
	Align: 8,
}

var TyLDouble = &CType{
	Kind:  TY_LDOUBLE,
	Size:  16,
	Align: 16,
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

func (t *CType) isFloat() bool {
	return t.Kind == TY_FLOAT || t.Kind == TY_DOUBLE || t.Kind == TY_LDOUBLE
}

func (t *CType) isNumeric() bool {
	return t.isInteger() || t.isFloat()
}

func pointerTo(base *CType) *CType {
	ty := newType(TY_PTR, 8, 8) // Assuming pointer size is 8 bytes
	ty.Base = base
	ty.IsUnsigned = true
	return ty
}

func funcType(returnTy *CType) *CType {
	// The C spec disallows sizeof(<function type>), but
	// GCC allows that and the expression is evaluated to 1.
	ty := newType(TY_FUNC, 1, 1)
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

	if ty.Kind == TY_FUNC {
		return pointerTo(ty)
	}
	if other.Kind == TY_FUNC {
		return pointerTo(other)
	}

	if ty.Kind == TY_LDOUBLE || other.Kind == TY_LDOUBLE {
		return TyLDouble
	}

	if ty.Kind == TY_DOUBLE || other.Kind == TY_DOUBLE {
		return TyDouble
	}

	if ty.Kind == TY_FLOAT || other.Kind == TY_FLOAT {
		return TyFloat
	}

	if ty.Size < 4 {
		ty = TyInt
	}
	if other.Size < 4 {
		other = TyInt
	}

	if ty.Size != other.Size {
		if ty.Size > other.Size {
			return ty
		} else {
			return other
		}
	}

	if other.IsUnsigned {
		return other
	}

	return ty
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

	switch node.Kind {
	case ND_NUM:
		node.Ty = TyInt
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
		if node.Ty == nil {
			panic("node.Ty == nil")
		}
		return
	case ND_NOT, ND_LOGAND, ND_LOGOR:
		node.Ty = TyInt
		return
	case ND_BITNOT, ND_SHL, ND_SHR:
		if !node.Lhs.Ty.isInteger() {
			errorTok(node.Lhs.Tok, "invalid operand")
		}
		intPromotion(&node.Lhs)
		node.Ty = node.Lhs.Ty
		return
	case ND_VAR:
		node.Ty = node.Variable.Ty
		return
	case ND_COND:
		if node.Then.Ty.Kind == TY_VOID || node.Else.Ty.Kind == TY_VOID {
			node.Ty = TyVoid
		} else {
			usualArithConv(&node.Then, &node.Else)
			node.Ty = node.Then.Ty
		}
		return
	case ND_COMMA:
		node.Ty = node.Rhs.Ty
		return
	case ND_MEMBER:
		node.Ty = node.Member.Ty
		return
	case ND_ADDR:
		node.Ty = pointerTo(node.Lhs.Ty)
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
	case ND_LABEL_VAL:
		node.Ty = pointerTo(TyVoid)
		return
	case ND_CAS:
		node.CasAddr.addType()
		node.CasOld.addType()
		node.CasNew.addType()
		node.Ty = TyBool

		if node.CasAddr.Ty.Kind != TY_PTR {
			errorTok(node.CasAddr.Tok, "pointer expected")
		}
		if node.CasOld.Ty.Kind != TY_PTR {
			errorTok(node.CasOld.Tok, "pointer expected")
		}

		return
	case ND_EXCH:
		if node.Lhs.Ty.Kind != TY_PTR {
			errorTok(node.CasOld.Tok, "pointer expected")
		}
		node.Ty = node.Lhs.Ty.Base
		return
	}
}

func (t1 *CType) isCompatibleWith(t2 *CType) bool {
	if t1 == t2 {
		return true
	}

	if t1.Origin != nil {
		return t1.Origin.isCompatibleWith(t2)
	}

	if t2.Origin != nil {
		return t1.isCompatibleWith(t2.Origin)
	}

	if t1.Kind != t2.Kind {
		return false
	}

	switch t1.Kind {
	case TY_CHAR, TY_SHORT, TY_INT, TY_LONG:
		return t1.IsUnsigned == t2.IsUnsigned
	case TY_FLOAT, TY_DOUBLE, TY_LDOUBLE:
		return true
	case TY_PTR:
		return t1.Base.isCompatibleWith(t2.Base)
	case TY_FUNC:
		if !t1.ReturnType.isCompatibleWith(t2.ReturnType) {
			return false
		}
		if t1.IsVariadic != t2.IsVariadic {
			return false
		}

		p1 := t1.ParamList
		p2 := t2.ParamList
		for p1 != nil && p2 != nil {
			if !p1.Ty.isCompatibleWith(p2.Ty) {
				return false
			}

			p1 = p1.ParamNext
			p2 = p2.ParamNext
		}

		return p1 == nil && p2 == nil
	case TY_ARRAY:
		if !t1.Base.isCompatibleWith(t2.Base) {
			return false
		}
		return t1.ArrayLength < 0 && t2.ArrayLength < 0 && t1.ArrayLength == t2.ArrayLength
	}

	return false
}

func (node *AstNode) isBitField() bool {
	return node.Kind == ND_MEMBER && node.Member.IsBitfield
}

func (t *CType) IntRank() int {
	switch t.Kind {
	case TY_ENUM, TY_BOOL, TY_CHAR, TY_SHORT:
		return 0
	case TY_INT:
		return 1
	case TY_LONG:
		return 2
	}
	panic("unreachable")
}

func intPromotion(node **AstNode) {
	ty := (*node).Ty

	if (*node).isBitField() {
		intWidth := TyInt.Size * 8
		bitWidth := (*node).Member.BitWidth

		if bitWidth == intWidth && ty.IsUnsigned {
			*node = newCast(*node, TyUInt)
		} else if bitWidth <= intWidth {
			*node = newCast(*node, TyInt)
		} else {
			*node = newCast(*node, ty)
		}

		return
	}

	if ty.Size < TyInt.Size {
		*node = newCast(*node, TyInt)
		return
	}

	if ty.Size == TyInt.Size && ty.IntRank() < TyInt.IntRank() {
		if ty.IsUnsigned {
			*node = newCast(*node, TyUInt)
		} else {
			*node = newCast(*node, TyInt)
		}

		return
	}
}
