package main

import (
	"fmt"
	"math"
)

const FP_MAX = 8
const GP_MAX = 6

var cgOutputFile *[]string
var argreg8 = []string{
	"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b",
}
var argreg16 = []string{
	"%di", "%si", "%dx", "%cx", "%r8w", "%r9w",
}
var argreg32 = []string{
	"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d",
}
var argreg64 = []string{
	"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9",
}

var LabelCount int = 1

var currentFn *Obj

var DontReuseStack bool

var FileNoInCg int
var LineNoInCg int

var LocalVarPointer string

func printLoc(tok *Token) {
	if FileNoInCg == tok.DisplayFileNo && LineNoInCg == tok.DisplayLineNo {
		return
	}

	printlnToFile("  .loc %d %d", tok.DisplayFileNo, tok.DisplayLineNo)

	FileNoInCg = tok.DisplayFileNo
	LineNoInCg = tok.DisplayLineNo
}

func printlnToFile(fmtStr string, args ...any) {
	code := fmt.Sprintf(fmtStr, args...)
	*cgOutputFile = append(*cgOutputFile, code)
}

type TmpStack struct {
	Data     []int
	Capacity int
	Depth    int
	Bottom   int
}

var tmpStack = TmpStack{}

func pushTmpStack() int {
	if tmpStack.Depth == tmpStack.Capacity {
		tmpStack.Capacity += 4
		for range 4 {
			tmpStack.Data = append(tmpStack.Data, 0)
		}
	}

	var offset int
	if !DontReuseStack {
		bottom := currentFn.LocalVarStackSize + (int64(tmpStack.Depth)+1)*8
		tmpStack.Bottom = int(math.Max(float64(tmpStack.Bottom), float64(bottom)))
		offset = -int(bottom)
	} else {
		tmpStack.Bottom += 8
		offset = -tmpStack.Bottom
	}

	tmpStack.Data[tmpStack.Depth] = offset
	tmpStack.Depth++
	return offset
}

func popTmpStack() int {
	tmpStack.Depth--
	return tmpStack.Data[tmpStack.Depth]
}

func pushTmp() int {
	offset := pushTmpStack()
	printlnToFile("  mov %%rax, %d(%s)", offset, LocalVarPointer)
	return offset
}

func popTmp(arg string) {
	offset := popTmpStack()
	printlnToFile("  mov %d(%s), %s", offset, LocalVarPointer, arg)
}

func pushTmpF() {
	offset := pushTmpStack()
	printlnToFile("  movsd %%xmm0, %d(%s)", offset, LocalVarPointer)
}

func popTmpF(reg int) {
	offset := popTmpStack()
	printlnToFile("  movsd %d(%s), %%xmm%d", offset, LocalVarPointer, reg)
}

// When we load a char or a short value to a register, we always
// extend them to the size of int, so we can assume the lower half of
// a register always contains a valid value.
func loadExtendInt(ty *CType, offset int, ptr string, reg string) {
	insn := "movs"
	if ty.IsUnsigned {
		insn = "movz"
	}

	switch ty.Size {
	case 1:
		printlnToFile("  %sbl %d(%s), %s", insn, offset, ptr, reg)
		return
	case 2:
		printlnToFile("  %swl %d(%s), %s", insn, offset, ptr, reg)
		return
	case 4:
		printlnToFile("  movl %d(%s), %s", offset, ptr, reg)
		return
	case 8:
		printlnToFile("  mov %d(%s), %s", offset, ptr, reg)
		return
	}
	panic("unreachable")
}

func callingConvention(v *Obj, gpStart int64, gpCount *int, fpCount *int, stackAlign *int) int64 {
	stack := int64(0)
	maxAlign := 16
	gp := gpStart
	fp := int64(0)
	for ; v != nil; v = v.ParamNext {
		ty := v.Ty
		if ty.Size == 0 {
			panic("ty.Size == 0")
		}

		switch ty.Kind {
		case TY_STRUCT, TY_UNION:
			if ty.Size <= 16 {
				fpInc := boolToInt(ty.hasFloatNumber1()) + boolToInt(ty.Size > 8 && ty.hasFloatNumber2())
				gpInc := boolToInt(!ty.hasFloatNumber1()) + boolToInt(ty.Size > 8 && !ty.hasFloatNumber2())

				if (fpInc == 0 || fp+int64(fpInc) <= FP_MAX) && (gpInc == 0 || gp+int64(gpInc) <= GP_MAX) {
					fp += int64(fpInc)
					gp += int64(gpInc)
					continue
				}
			}
		case TY_FLOAT, TY_DOUBLE:
			if fp < FP_MAX {
				fp++
				continue
			}
		case TY_LDOUBLE: // DO NOTHING
		default:
			if gp < GP_MAX {
				gp++
				continue
			}
		}

		v.PassByStack = true

		if ty.Align > 8 {
			stack = alignTo(stack, ty.Align)
			maxAlign = int(math.Max(float64(maxAlign), float64(ty.Align)))
		}
		v.StackOffset = stack
		stack += alignTo(ty.Size, 8)
	}

	if gpCount != nil {
		*gpCount = int(math.Min(float64(gp), GP_MAX))
	}
	if fpCount != nil {
		*fpCount = int(math.Min(float64(fp), FP_MAX))
	}
	if stackAlign != nil {
		*stackAlign = maxAlign
	}

	return stack
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
//   - Up to 6 arguments of integral type are passed using RDI, RSI,
//     RDX, RCX, R8 and R9.
//
//   - Up to 8 arguments of floating-point type are passed using XMM0 to
//     XMM7.
//
//   - If all registers of an appropriate type are already used, push an
//     argument to the stack in the right-to-left order.
//
//   - Each argument passed on the stack takes 8 bytes, and the end of
//     the argument area must be aligned to a 16 byte boundary.
//
//   - If a function is variadic, set the number of floating-point type
//     arguments to RAX.
func placeStackArgs(node *AstNode) {
	for v := node.Args; v != nil; v = v.ParamNext {
		if !v.PassByStack {
			continue
		}

		switch v.Ty.Kind {
		case TY_STRUCT, TY_UNION:
			for i := int64(0); i < v.Ty.Size; i++ {
				printlnToFile("  mov %d(%s), %%r8b", i+v.Offset, v.Pointer)
				printlnToFile("  mov %%r8b, %d(%%rsp)", i+v.StackOffset)
			}
			continue
		case TY_FLOAT, TY_DOUBLE:
			printlnToFile("  movsd %d(%s), %%xmm0", v.Offset, v.Pointer)
			printlnToFile("  movsd %%xmm0, %d(%%rsp)", v.StackOffset)
			continue
		case TY_LDOUBLE:
			printlnToFile("  fldt %d(%s)", v.Offset, v.Pointer)
			printlnToFile("  fstpt %d(%%rsp)", v.StackOffset)
			continue
		}

		ax := "%rax"
		if v.Ty.Size <= 4 {
			ax = "%eax"
		}
		loadExtendInt(v.Ty, int(v.Offset), v.Pointer, ax)
		printlnToFile("  mov %%rax, %d(%%rsp)", v.StackOffset)
	}
}

func placeRegArgs(node *AstNode, gpStart bool) {
	gp := 0
	fp := 0
	// If the return type is a large struct/union, the caller passes
	// a pointer to a buffer as if it were the first argument.
	if gpStart {
		printlnToFile("  lea %d(%s), %s", node.ReturnBuffer.Offset, node.ReturnBuffer.Pointer, argreg64[gp])
		gp++
	}

	for v := node.Args; v != nil; v = v.ParamNext {
		if v.PassByStack {
			continue
		}

		switch v.Ty.Kind {
		case TY_STRUCT, TY_UNION:
			if v.Ty.hasFloatNumber1() {
				printlnToFile("  movsd %d(%s), %%xmm%d", v.Offset, v.Pointer, fp)
				fp++
			} else {
				printlnToFile("  mov %d(%s), %s", v.Offset, v.Pointer, argreg64[gp])
				gp++
			}

			if v.Ty.Size > 8 {
				if v.Ty.hasFloatNumber2() {
					printlnToFile("  movsd %d(%s), %%xmm%d", 8+v.Offset, v.Pointer, fp)
					fp++
				} else {
					printlnToFile("  mov %d(%s), %s", 8+v.Offset, v.Pointer, argreg64[gp])
					gp++
				}
			}
			continue
		case TY_FLOAT:
			printlnToFile("  movss %d(%s), %%xmm%d", v.Offset, v.Pointer, fp)
			fp++
			continue
		case TY_DOUBLE:
			printlnToFile("  movsd %d(%s), %%xmm%d", v.Offset, v.Pointer, fp)
			fp++
			continue
		}

		argreg := argreg64[gp]
		if v.Ty.Size <= 4 {
			argreg = argreg32[gp]
		}
		gp++
		loadExtendInt(v.Ty, int(v.Offset), v.Pointer, argreg)
	}
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
func (ty *CType) hasFloatNumber(lo int64, hi int64, offset int64) bool {
	if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
		for mem := ty.Members; mem != nil; mem = mem.Next {
			if !mem.Ty.hasFloatNumber(lo, hi, offset+mem.Offset) {
				return false
			}
		}
		return true
	}

	if ty.Kind == TY_ARRAY {
		for i := int64(0); i < ty.ArrayLength; i += 1 {
			if !ty.Base.hasFloatNumber(lo, hi, offset+ty.Base.Size*i) {
				return false
			}
		}
		return true
	}

	return offset < lo || hi <= offset || ty.Kind == TY_FLOAT || ty.Kind == TY_DOUBLE
}

func (ty *CType) hasFloatNumber1() bool {
	return ty.hasFloatNumber(0, 8, 0)
}

func (ty *CType) hasFloatNumber2() bool {
	return ty.hasFloatNumber(8, 16, 0)
}

func boolToInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

func (v *Obj) copyReturnBuffer() {
	ty := v.Ty
	gp := 0
	fp := 0

	if ty.hasFloatNumber1() {
		if !(ty.Size == 4 || 8 <= ty.Size) {
			panic("")
		}
		if ty.Size == 4 {
			printlnToFile("  movss %%xmm0, %d(%s)", v.Offset, v.Pointer)
		} else {
			printlnToFile("  movsd %%xmm0, %d(%s)", v.Offset, v.Pointer)
		}
		fp += 1
	} else {
		for i := int64(0); i < int64(math.Min(8, float64(ty.Size))); i += 1 {
			printlnToFile("  mov %%al, %d(%s)", v.Offset+i, v.Pointer)
			printlnToFile("  shr $8, %%rax")
		}
		gp += 1
	}

	if ty.Size > 8 {
		if ty.hasFloatNumber2() {
			if !(ty.Size == 12 || ty.Size == 16) {
				panic("")
			}

			if ty.Size == 12 {
				printlnToFile("  movss %%xmm%d, %d(%s)", fp, v.Offset+8, v.Pointer)
			} else {
				printlnToFile("  movsd %%xmm%d, %d(%s)", fp, v.Offset+8, v.Pointer)
			}
		} else {
			reg1 := "%al"
			reg2 := "%rax"
			if gp != 0 {
				reg1 = "%dl"
				reg2 = "%rdx"
			}
			for i := int64(8); i < int64(math.Min(16, float64(ty.Size))); i += 1 {
				printlnToFile("  mov %s, %d(%s)", reg1, v.Offset+i, v.Pointer)
				printlnToFile("  shr $8, %s", reg2)
			}
		}
	}
}

func copyStructReg() {
	ty := currentFn.Ty.ReturnType
	gp := 0
	fp := 0

	printlnToFile("  mov %%rax, %%rdi")

	if ty.hasFloatNumber1() {
		if !(ty.Size == 4 || 8 <= ty.Size) {
			panic("")
		}
		if ty.Size == 4 {
			printlnToFile("  movss (%%rdi), %%xmm0")
		} else {
			printlnToFile("  movsd (%%rdi), %%xmm0")
		}
		fp += 1
	} else {
		printlnToFile("  mov $0, %%rax")
		for i := int64(math.Min(8, float64(ty.Size))) - 1; i >= 0; i -= 1 {
			printlnToFile("  shl $8, %%rax")
			printlnToFile("  mov %d(%%rdi), %%al", i)
		}
		gp += 1
	}

	if ty.Size > 8 {
		if ty.hasFloatNumber2() {
			if !(ty.Size == 12 || ty.Size == 16) {
				panic("type size must be 12 or 16")
			}

			if ty.Size == 12 {
				printlnToFile("  movss 8(%%rdi), %%xmm%d", fp)
			} else {
				printlnToFile("  movsd 8(%%rdi), %%xmm%d", fp)
			}
		} else {
			reg1 := "%al"
			reg2 := "%rax"
			if gp != 0 {
				reg1 = "%dl"
				reg2 = "%rdx"
			}
			printlnToFile("  mov $0, %s", reg2)
			for i := int64(math.Min(16, float64(ty.Size))) - 1; i >= 8; i -= 1 {
				printlnToFile("  shl $8, %s", reg2)
				printlnToFile("  mov %d(%%rdi), %s", i, reg1)
			}
		}
	}
}

func copyStructMem() {
	ty := currentFn.Ty.ReturnType
	v := currentFn.Ty.ParamList

	printlnToFile("  mov %d(%s), %%rdi", v.Offset, v.Pointer)

	for i := int64(0); i < ty.Size; i += 1 {
		printlnToFile("  mov %d(%%rax), %%dl", i)
		printlnToFile("  mov %%dl, %d(%%rdi)", i)
	}
	printlnToFile("  mov %%rdi, %%rax")
}

func count() int {
	LabelCount += 1
	return LabelCount - 1
}

func regDX(sz int) string {
	switch sz {
	case 1:
		return "%dl"
	case 2:
		return "%dx"
	case 4:
		return "%edx"
	case 8:
		return "%rdx"
	}
	panic("unreachable")
}

func regAX(sz int) string {
	switch sz {
	case 1:
		return "%al"
	case 2:
		return "%ax"
	case 4:
		return "%eax"
	case 8:
		return "%rax"
	}
	panic("unreachable")
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
func alignTo(n int64, align int64) int64 {
	return (n + align - 1) / align * align
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func genAddr(node *AstNode) {
	switch node.Kind {
	case ND_VAR:
		// Variable-length array, which is always local.
		if node.Variable.Ty.Kind == TY_VLA {
			printlnToFile("  mov %d(%s), %%rax", node.Variable.Offset, node.Variable.Pointer)
			return
		}

		// Local variable
		if node.Variable.IsLocal {
			printlnToFile("  lea %d(%s), %%rax", node.Variable.Offset, node.Variable.Pointer)
			return
		}

		if opt_fpic {
			// Thread-local variable
			if node.Variable.IsTls {
				printlnToFile("  data16 lea %s@tlsgd(%%rip), %%rdi", node.Variable.Name)
				printlnToFile("  .value 0x6666")
				printlnToFile("  rex64")
				printlnToFile("  call __tls_get_addr@PLT")
				return
			}

			// Function or global variable
			printlnToFile("  mov %s@GOTPCREL(%%rip), %%rax", node.Variable.Name)
			return
		}

		// Thread-local variable
		if node.Variable.IsTls {
			printlnToFile("  mov %%fs:0, %%rax")
			printlnToFile("  add $%s@tpoff, %%rax", node.Variable.Name)
			return
		}
		// Here, we generate an absolute address of a function or a global
		// variable. Even though they exist at a certain address at runtime,
		// their addresses are not known at link-time for the following
		// two reasons.
		//
		//  - Address randomization: Executables are loaded to memory as a
		//    whole but it is not known what address they are loaded to.
		//    Therefore, at link-time, relative address in the same
		//    exectuable (i.e. the distance between two functions in the
		//    same executable) is known, but the absolute address is not
		//    known.
		//
		//  - Dynamic linking: Dynamic shared objects (DSOs) or .so files
		//    are loaded to memory alongside an executable at runtime and
		//    linked by the runtime loader in memory. We know nothing
		//    about addresses of global stuff that may be defined by DSOs
		//    until the runtime relocation is complete.
		//
		// In order to deal with the former case, we use RIP-relative
		// addressing, denoted by `(%rip)`. For the latter, we obtain an
		// address of a stuff that may be in a shared object file from the
		// Global Offset Table using `@GOTPCREL(%rip)` notation.

		// Function
		if node.Ty.Kind == TY_FUNC {
			if node.Variable.IsDefinition {
				printlnToFile("  lea %s(%%rip), %%rax", node.Variable.Name)
			} else {
				printlnToFile("  mov %s@GOTPCREL(%%rip), %%rax", node.Variable.Name)
			}
			return
		}

		// Global variable
		printlnToFile("  lea %s(%%rip), %%rax", node.Variable.Name)
		return
	case ND_DEREF:
		genExpr(node.Lhs)
		return
	case ND_COMMA:
		genExpr(node.Lhs)
		genAddr(node.Rhs)
		return
	case ND_MEMBER:
		switch node.Lhs.Kind {
		case ND_FUNCALL:
			if node.Lhs.ReturnBuffer == nil {
				// DO NOTHING
			} else if node.Lhs.Ty.Kind != TY_STRUCT && node.Lhs.Ty.Kind != TY_UNION {
				// DO NOTHING
			} else {
				genExpr(node.Lhs)
				printlnToFile("  add $%d, %%rax", node.Member.Offset)
				return
			}
		case ND_ASSIGN, ND_COND, ND_STMT_EXPR:
			if node.Lhs.Ty.Kind != TY_STRUCT && node.Lhs.Ty.Kind != TY_UNION {
				// DO NOTHING
			} else {
				genExpr(node.Lhs)
				printlnToFile("  add $%d, %%rax", node.Member.Offset)
				return
			}
		default:
			genAddr(node.Lhs)
			printlnToFile("  add $%d, %%rax", node.Member.Offset)
			return
		}
	}

	errorTok(node.Tok, "not an lvalue")
}

// Load a value from where %rax is pointing to.
func load(ty *CType) {
	if ty.Kind == TY_ARRAY || ty.Kind == TY_STRUCT || ty.Kind == TY_UNION || ty.Kind == TY_FUNC || ty.Kind == TY_VLA {
		// If it is an array, do not attempt to load a value to the
		// register because in general we can't load an entire array to a
		// register. As a result, the result of an evaluation of an array
		// becomes not the array itself but the address of the array.
		// This is where "array is automatically converted to a pointer to
		// the first element of the array in C" occurs.
		return
	}

	if ty.Kind == TY_FLOAT {
		printlnToFile("  movss (%%rax), %%xmm0")
		return
	}

	if ty.Kind == TY_DOUBLE {
		printlnToFile("  movsd (%%rax), %%xmm0")
		return
	}

	if ty.Kind == TY_LDOUBLE {
		printlnToFile("  fldt (%%rax)")
		return
	}

	ax := "%rax"
	if ty.Size <= 4 {
		ax = "%eax"
	}
	loadExtendInt(ty, 0, "%rax", ax)
}

// Store %rax to an address that the stack top is pointing to.
func store(ty *CType) {
	popTmp("%rdi")

	if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
		for i := int64(0); i < ty.Size; i += 1 {
			printlnToFile("  mov %d(%%rax), %%r8b", i)
			printlnToFile("  mov %%r8b, %d(%%rdi)", i)
		}
		return
	}

	if ty.Kind == TY_FLOAT {
		printlnToFile("  movss %%xmm0, (%%rdi)")
		return
	}

	if ty.Kind == TY_DOUBLE {
		printlnToFile("  movsd %%xmm0, (%%rdi)")
		return
	}

	if ty.Kind == TY_LDOUBLE {
		printlnToFile("  fstpt (%%rdi)")
		return
	}

	if ty.Size == 1 {
		printlnToFile("  mov %%al, (%%rdi)")
	} else if ty.Size == 2 {
		printlnToFile("  mov %%ax, (%%rdi)")
	} else if ty.Size == 4 {
		printlnToFile("  mov %%eax, (%%rdi)")
	} else {
		printlnToFile("  mov %%rax, (%%rdi)")
	}
}

func cmpZero(ty *CType) {
	if ty.Kind == TY_FLOAT {
		printlnToFile("  xorps %%xmm1, %%xmm1")
		printlnToFile("  ucomiss %%xmm1, %%xmm0")
		return
	}

	if ty.Kind == TY_DOUBLE {
		printlnToFile("  xorpd %%xmm1, %%xmm1")
		printlnToFile("  ucomisd %%xmm1, %%xmm0")
		return
	}

	if ty.Kind == TY_LDOUBLE {
		printlnToFile("  fldz")
		printlnToFile("  fucomip")
		printlnToFile("  fstp %%st(0)")
		return
	}

	if ty.isInteger() && ty.Size <= 4 {
		printlnToFile("  cmp $0, %%eax")
	} else {
		printlnToFile("  cmp $0, %%rax")
	}
}

const (
	I8 = iota
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	F32
	F64
	F80
)

func getTypeId(ty *CType) int {
	switch ty.Kind {
	case TY_CHAR, TY_PCHAR:
		if ty.IsUnsigned {
			return U8
		} else {
			return I8
		}
	case TY_SHORT:
		if ty.IsUnsigned {
			return U16
		} else {
			return I16
		}
	case TY_INT:
		if ty.IsUnsigned {
			return U32
		} else {
			return I32
		}
	case TY_LONG, TY_LONGLONG:
		if ty.IsUnsigned {
			return U64
		} else {
			return I64
		}
	case TY_FLOAT:
		return F32
	case TY_DOUBLE:
		return F64
	case TY_LDOUBLE:
		return F80
	}
	return U64
}

// The table for type casts
const I32I8 = "movsbl %al, %eax"
const I32U8 = "movzbl %al, %eax"
const I32I16 = "movswl %ax, %eax"
const I32U16 = "movzwl %ax, %eax"
const I32F32 = "cvtsi2ssl %eax, %xmm0"
const I32I64 = "movslq %eax, %rax"
const I32F64 = "cvtsi2sdl %eax, %xmm0"
const I32F80 = "mov %eax, -4(%rsp); fildl -4(%rsp)"

const U32F32 = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0"
const U32I64 = "mov %eax, %eax"
const U32F64 = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0"
const U32F80 = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)"

const I64F32 = "cvtsi2ssq %rax, %xmm0"
const I64F64 = "cvtsi2sdq %rax, %xmm0"
const I64F80 = "movq %rax, -8(%rsp); fildll -8(%rsp)"

const U64F32 = `test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2ss %rax,%xmm0; jmp 2f; 
  1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; 
  or %rax,%rdi; cvtsi2ss %rdi,%xmm0; addss %xmm0,%xmm0; 2:`
const U64F64 = `test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; 
                1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; 
                or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:`
const U64F80 = `mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;
                mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:`

const F32I8 = "cvttss2sil %xmm0, %eax; movsbl %al, %eax"
const F32U8 = "cvttss2sil %xmm0, %eax; movzbl %al, %eax"
const F32I16 = "cvttss2sil %xmm0, %eax; movswl %ax, %eax"
const F32U16 = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax"
const F32I32 = "cvttss2sil %xmm0, %eax"
const F32U32 = "cvttss2siq %xmm0, %rax"
const F32I64 = "cvttss2siq %xmm0, %rax"
const F32U64 = `cvttss2siq %xmm0, %rcx; movq %rcx, %rdx; movl $0x5F000000, %eax; 
  movd %eax, %xmm1; subss %xmm1, %xmm0; cvttss2siq %xmm0, %rax; 
  sarq $63, %rdx; andq %rdx, %rax; orq %rcx, %rax;`
const F32F64 = "cvtss2sd %xmm0, %xmm0"
const F32F80 = "movss %xmm0, -4(%rsp); flds -4(%rsp)"

const F64I8 = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax"
const F64U8 = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax"
const F64I16 = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax"
const F64U16 = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax"
const F64I32 = "cvttsd2sil %xmm0, %eax"
const F64U32 = "cvttsd2siq %xmm0, %rax"
const F64F32 = "cvtsd2ss %xmm0, %xmm0"
const F64I64 = "cvttsd2siq %xmm0, %rax"
const F64U64 = `cvttsd2siq %xmm0, %rcx; movq %rcx, %rdx; mov $0x43e0000000000000, %rax; 
  movq %rax, %xmm1; subsd %xmm1, %xmm0; cvttsd2siq %xmm0, %rax; 
  sarq $63, %rdx; andq %rdx, %rax; orq %rcx, %rax`
const F64F80 = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)"

const FROM_F80_1 = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " + "mov %ax, -12(%rsp); fldcw -12(%rsp); "
const FROM_F80_2 = " -24(%rsp); fldcw -10(%rsp); "

const F80I8 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movsbl -24(%rsp), %eax"
const F80U8 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl -24(%rsp), %eax"
const F80I16 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl -24(%rsp), %eax"
const F80U16 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "movswl -24(%rsp), %eax"
const F80I32 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov -24(%rsp), %eax"
const F80U32 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov -24(%rsp), %eax"
const F80I64 = FROM_F80_1 + "fistpq" + FROM_F80_2 + "mov -24(%rsp), %rax"
const F80U64 = FROM_F80_1 + "fistpq" + FROM_F80_2 + "mov -24(%rsp), %rax"
const F80F32 = "fstps -8(%rsp); movss -8(%rsp), %xmm0"
const F80F64 = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0"

var CastTable = [][]string{
	// i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 f80
	{"", "", "", I32I64, I32U8, I32I16, "", I32I64, I32F32, I32F64, I32F80},            // i8
	{I32I8, "", "", I32I64, I32U8, I32U16, "", I32I64, I32F32, I32F64, I32F80},         // i16
	{I32I8, I32I16, "", I32I64, I32U8, I32U16, "", I32I64, I32F32, I32F64, I32F80},     // i32
	{I32I8, I32I16, "", "", I32U8, I32U16, "", "", I64F32, I64F64, I64F80},             // i64
	{I32I8, "", "", I32I64, "", "", "", I32I64, I32F32, I32F64, I32F80},                // u8
	{I32I8, I32I16, "", I32I64, I32U8, "", "", I32I64, I32F32, I32F64, I32F80},         // u16
	{I32I8, I32I16, "", U32I64, I32U8, I32U16, "", U32I64, U32F32, U32F64, U32F80},     // u32
	{I32I8, I32I16, "", "", I32U8, I32U16, "", "", U64F32, U64F64, U64F80},             // u64
	{F32I8, F32I16, F32I32, F32I64, F32U8, F32U16, F32U32, F32U64, "", F32F64, F32F80}, //f32
	{F64I8, F64I16, F64I32, F64I64, F64U8, F64U16, F64U32, F64U64, F64F32, "", F64F80}, //f64
	{F80I8, F80I16, F80I32, F80I64, F80U8, F80U16, F80U32, F80U64, F80F32, F80F64, ""}, // f80
}

func cast(from *CType, to *CType) {
	if to.Kind == TY_VOID {
		return
	}

	if to.Kind == TY_BOOL {
		cmpZero(from)
		printlnToFile("  setne %%al")
		printlnToFile("  movzx %%al, %%eax")
		return
	}

	t1 := getTypeId(from)
	t2 := getTypeId(to)
	if CastTable[t1][t2] != "" {
		printlnToFile("  %s", CastTable[t1][t2])
	}
}

func storeFp(r int, sz int64, offset int64, ptr string) {
	if sz == 4 {
		printlnToFile("  movss %%xmm%d, %d(%s)", r, offset, ptr)
		return
	}
	if sz == 8 {
		printlnToFile("  movsd %%xmm%d, %d(%s)", r, offset, ptr)
		return
	}
	panic("unreachable")
}

func storeGp(r int, sz int64, offset int64, ptr string) {
	switch sz {
	case 1:
		printlnToFile("  mov %s, %d(%s)", argreg8[r], offset, ptr)
		return
	case 2:
		printlnToFile("  mov %s, %d(%s)", argreg16[r], offset, ptr)
		return
	case 4:
		printlnToFile("  mov %s, %d(%s)", argreg32[r], offset, ptr)
		return
	case 8:
		printlnToFile("  mov %s, %d(%s)", argreg64[r], offset, ptr)
		return
	default:
		for i := int64(0); i < sz; i += 1 {
			printlnToFile("  mov %s, %d(%s)", argreg8[r], offset+i, ptr)
			printlnToFile("  shr $8, %s", argreg64[r])
		}
		return
	}
}

// Generate code for a given node.
func genExpr(node *AstNode) {
	if opt_g {
		printLoc(node.Tok)
	}

	switch node.Kind {
	case ND_NULL_EXPR:
		return
	case ND_LABEL_VAL:
		printlnToFile("  lea %s(%%rip), %%rax", node.UniqueLabel)
		return
	case ND_VA_ARG:
		genExpr(node.Lhs)
		ty := node.Ty
		v := node.Variable

		if ty.Size <= 16 {
			regClass0 := !ty.hasFloatNumber1()
			regClass1 := false
			if ty.Size > 8 {
				regClass1 = !ty.hasFloatNumber2()
			}

			gpInc := boolToInt(regClass0) + boolToInt(ty.Size > 8 && regClass1)
			if gpInc != 0 {
				printlnToFile("  cmpl $%d, (%%rax)", 48-gpInc*8)
				printlnToFile("  ja 1f")
			}
			fpInc := boolToInt(!regClass0) + boolToInt(ty.Size > 8 && !regClass1)
			if fpInc != 0 {
				printlnToFile("  cmpl $%d, 4(%%rax)", 176-fpInc*16)
				printlnToFile("  ja 1f")
			}
			for i := 0; i < (int(ty.Size)+7)/8; i++ {
				if (i == 0 && regClass0) || (i == 1 && regClass1) {
					printlnToFile("  movl (%%rax), %%edi")   // gp_offset
					printlnToFile("  addq 16(%%rax), %%rdi") // reg_save_area
					printlnToFile("  addq $8, (%%rax)")
				} else {
					printlnToFile("  movl 4(%%rax), %%edi")  // fp_offset
					printlnToFile("  addq 16(%%rax), %%rdi") // reg_save_area
					printlnToFile("  addq $16, 4(%%rax)")
				}
				for ofs := 0; ofs < (int(ty.Size) - i*8); ofs++ {
					printlnToFile("  mov %d(%%rdi), %%r8b", ofs)
					printlnToFile("  mov %%r8b, %d(%s)", ofs+i*8+int(v.Offset), v.Pointer)
				}
			}
			printlnToFile("  jmp 2f")
			printlnToFile("1:")
		}

		printlnToFile("  movq 8(%%rax), %%rdi") // overflow_arg_area
		if ty.Align > 8 {
			printlnToFile("  addq $%d, %%rdi", ty.Align-1)
			printlnToFile("  andq $-%d, %%rdi", ty.Align)
		}
		printlnToFile("  movq %%rdi, %%rdx")
		printlnToFile("  addq $%d, %%rdx", alignTo(ty.Size, 8))
		printlnToFile("  movq %%rdx, 8(%%rax)")
		for ofs := int64(0); ofs < ty.Size; ofs++ {
			printlnToFile("  mov %d(%%rdi), %%r8b", ofs)
			printlnToFile("  mov %%r8b, %d(%s)", ofs+v.Offset, v.Pointer)
		}
		if ty.Size <= 16 {
			printlnToFile("2:")
		}
		return
	case ND_EXCH:
		genExpr(node.Lhs)
		pushTmp()
		genExpr(node.Rhs)
		popTmp("%rdi")

		sz := int(node.Lhs.Ty.Base.Size)
		printlnToFile("  xchg %s, (%%rdi)", regAX(sz))
		return
	case ND_CAS:
		genExpr(node.CasAddr)
		pushTmp()
		genExpr(node.CasNew)
		pushTmp()
		genExpr(node.CasOld)
		printlnToFile("  mov %%rax, %%r8")
		load(node.CasOld.Ty.Base)
		popTmp("%rdx") // new
		popTmp("%rdi") // addr

		sz := int(node.CasAddr.Ty.Base.Size)
		printlnToFile("  lock cmpxchg %s, (%%rdi)", regDX(sz))
		printlnToFile("  sete %%cl")
		printlnToFile("  je 1f")
		printlnToFile("  mov %s, (%%r8)", regAX(sz))
		printlnToFile("1:")
		printlnToFile("  movzbl %%cl, %%eax")
		return
	case ND_NUM:
		switch node.Ty.Kind {
		case TY_FLOAT:
			u := math.Float32bits(float32(node.FloatValue))
			printlnToFile("  mov $%d, %%eax  # float %f", u, node.FloatValue)
			printlnToFile("  movq %%rax, %%xmm0")
			return
		case TY_DOUBLE:
			u := math.Float64bits(node.FloatValue)
			printlnToFile("  mov $%d, %%rax  # double %f", u, node.FloatValue)
			printlnToFile("  movq %%rax, %%xmm0")
			return
		case TY_LDOUBLE:
			u := NewFromFloat64(node.FloatValue)
			printlnToFile("  mov $%d, %%rax  # long double %f", u.m, node.FloatValue)
			printlnToFile("  mov %%rax, -16(%%rsp)")
			printlnToFile("  mov $%d, %%rax", u.se)
			printlnToFile("  mov %%rax, -8(%%rsp)")
			printlnToFile("  fldt -16(%%rsp)")
			return
		}
		printlnToFile("  mov $%d, %%rax", node.Value)
		return
	case ND_POS:
		genExpr(node.Lhs)
		return
	case ND_NEG:
		genExpr(node.Lhs)

		if node.Ty.Kind == TY_FLOAT {
			printlnToFile("  mov $1, %%rax")
			printlnToFile("  shl $31, %%rax")
			printlnToFile("  movq %%rax, %%xmm1")
			printlnToFile("  xorps %%xmm1, %%xmm0")
			return
		}

		if node.Ty.Kind == TY_DOUBLE {
			printlnToFile("  mov $1, %%rax")
			printlnToFile("  shl $63, %%rax")
			printlnToFile("  movq %%rax, %%xmm1")
			printlnToFile("  xorpd %%xmm1, %%xmm0")
			return
		}

		if node.Ty.Kind == TY_LDOUBLE {
			printlnToFile("  fchs")
			return
		}

		printlnToFile("  neg %%rax")
		return
	case ND_VAR:
		genAddr(node)
		load(node.Ty)
		return
	case ND_MEMBER:
		genAddr(node)
		load(node.Ty)

		mem := node.Member
		if mem.IsBitfield {
			printlnToFile("  shl $%d, %%rax", 64-mem.BitWidth-mem.BitOffset)
			if mem.Ty.IsUnsigned {
				printlnToFile("  shr $%d, %%rax", 64-mem.BitWidth)
			} else {
				printlnToFile("  sar $%d, %%rax", 64-mem.BitWidth)
			}
		}
		return
	case ND_DEREF:
		genExpr(node.Lhs)
		load(node.Ty)
		return
	case ND_ADDR:
		genAddr(node.Lhs)
		return
	case ND_ASSIGN:
		genAddr(node.Lhs)
		tmpOffset := pushTmp()
		genExpr(node.Rhs)
		if node.Lhs.isBitField() {
			// If the lhs is a bitfield, we need to read the current value
			// from memory and merge it with a new value.
			mem := node.Lhs.Member
			printlnToFile("  mov $%d, %%rdi", (1<<mem.BitWidth)-1)
			printlnToFile("  and %%rdi, %%rax")
			printlnToFile("  mov %%rax, %%r8")

			printlnToFile("  mov %d(%s), %%rax", tmpOffset, LocalVarPointer)
			load(mem.Ty)

			mask := ((1 << mem.BitWidth) - 1) << mem.BitOffset
			printlnToFile("  mov $%d, %%rdi", ^mask)
			printlnToFile("  and %%rdi, %%rax")

			printlnToFile("  mov %%r8, %%rdi")
			printlnToFile("  shl $%d, %%rdi", mem.BitOffset)
			printlnToFile("  or %%rdi, %%rax")
			store(node.Ty)
			printlnToFile("  mov %%r8, %%rax")

			if !mem.Ty.IsUnsigned {
				printlnToFile("  shl $%d, %%rax", 64-mem.BitWidth)
				printlnToFile("  sar $%d, %%rax", 64-mem.BitWidth)
			}
			return
		}

		store(node.Ty)
		return
	case ND_STMT_EXPR:
		for n := node.Body; n != nil; n = n.Next {
			genStmt(n)
		}
		dealloc_vla(node)
		return
	case ND_COMMA:
		genExpr(node.Lhs)
		genExpr(node.Rhs)
		return
	case ND_CAST:
		genExpr(node.Lhs)
		cast(node.Lhs.Ty, node.Ty)
		return
	case ND_MEMZERO:
		// `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
		printlnToFile("  mov $%d, %%rcx", node.Variable.Ty.Size)
		printlnToFile("  lea %d(%s), %%rdi", node.Variable.Offset, node.Variable.Pointer)
		printlnToFile("  xor %%al, %%al")
		printlnToFile("  rep stosb")
		return
	case ND_COND:
		c := count()
		genExpr(node.Cond)
		cmpZero(node.Cond.Ty)
		printlnToFile("  je .L.else.%d", c)
		genExpr(node.Then)
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.else.%d:", c)
		genExpr(node.Else)
		printlnToFile(".L.end.%d:", c)
		return
	case ND_NOT:
		genExpr(node.Lhs)
		cmpZero(node.Lhs.Ty)
		printlnToFile("  sete %%al")
		printlnToFile("  movzx %%al, %%rax")
		return
	case ND_BITNOT:
		genExpr(node.Lhs)
		printlnToFile("  not %%rax")
		return
	case ND_LOGAND:
		c := count()
		genExpr(node.Lhs)
		cmpZero(node.Lhs.Ty)
		printlnToFile("  je  .L.false.%d", c)
		genExpr(node.Rhs)
		cmpZero(node.Rhs.Ty)
		printlnToFile("  je  .L.false.%d", c)
		printlnToFile("  mov $1, %%rax")
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.false.%d:", c)
		printlnToFile("  mov $0, %%rax")
		printlnToFile(".L.end.%d:", c)
		return
	case ND_LOGOR:
		c := count()
		genExpr(node.Lhs)
		cmpZero(node.Lhs.Ty)
		printlnToFile("  jne .L.true.%d", c)
		genExpr(node.Rhs)
		cmpZero(node.Rhs.Ty)
		printlnToFile("  jne .L.true.%d", c)
		printlnToFile("  mov $0, %%rax")
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.true.%d:", c)
		printlnToFile("  mov $1, %%rax")
		printlnToFile(".L.end.%d:", c)
		return
	case ND_FUNCALL:
		if node.Lhs.Kind == ND_VAR && node.Lhs.Variable.Name == "alloca" {
			genExpr(node.ArgsExpr)
			builtin_alloca(node)
			return
		}

		printlnToFile("  mov %%rsp, %%rax")
		pushTmp()

		genExpr(node.Lhs)
		pushTmp()

		if node.ArgsExpr != nil {
			genExpr(node.ArgsExpr)
		}
		// If the return type is a large struct/union, the caller passes
		// a pointer to a buffer as if it were the first argument.
		gpStart := node.ReturnBuffer != nil && node.Ty.Size > 16

		fpCount := 0
		stackAlign := 0
		argsSize := callingConvention(node.Args, int64(boolToInt(gpStart)), nil, &fpCount, &stackAlign)

		printlnToFile("  sub $%d, %%rsp", argsSize)
		printlnToFile("  and $-%d, %%rsp", stackAlign)

		placeStackArgs(node)
		placeRegArgs(node, gpStart)

		printlnToFile("  mov $%d, %%rax", fpCount)
		popTmp("%r10")
		printlnToFile("  call *%%r10")

		popTmp("%rsp")

		// It looks like the most significant 48 or 56 bits in RAX may
		// contain garbage if a function return type is short or bool/char,
		// respectively. We clear the upper bits here.
		switch node.Ty.Kind {
		case TY_BOOL:
			printlnToFile("  movzx %%al, %%eax")
			return
		case TY_CHAR, TY_PCHAR:
			if node.Ty.IsUnsigned {
				printlnToFile("  movzbl %%al, %%eax")
			} else {
				printlnToFile("  movsbl %%al, %%eax")
			}
			return
		case TY_SHORT:
			if node.Ty.IsUnsigned {
				printlnToFile("  movzwl %%ax, %%eax")
			} else {
				printlnToFile("  movswl %%ax, %%eax")
			}
			return
		}

		// If the return type is a small struct, a value is returned
		// using up to two registers.
		if node.ReturnBuffer != nil && node.Ty.Size <= 16 {
			node.ReturnBuffer.copyReturnBuffer()
			printlnToFile("  lea %d(%s), %%rax", node.ReturnBuffer.Offset, node.ReturnBuffer.Pointer)
		}

		return
	}

	if node.Lhs.Ty.Kind == TY_FLOAT || node.Lhs.Ty.Kind == TY_DOUBLE {
		genExpr(node.Rhs)
		pushTmpF()
		genExpr(node.Lhs)
		popTmpF(1)

		sz := "sd"
		if node.Lhs.Ty.Kind == TY_FLOAT {
			sz = "ss"
		}

		switch node.Kind {
		case ND_ADD:
			printlnToFile("  add%s %%xmm1, %%xmm0", sz)
			return
		case ND_SUB:
			printlnToFile("  sub%s %%xmm1, %%xmm0", sz)
			return
		case ND_MUL:
			printlnToFile("  mul%s %%xmm1, %%xmm0", sz)
			return
		case ND_DIV:
			printlnToFile("  div%s %%xmm1, %%xmm0", sz)
			return
		case ND_EQ, ND_NE, ND_LT, ND_LE:
			printlnToFile("  ucomi%s %%xmm0, %%xmm1", sz)

			if node.Kind == ND_EQ {
				printlnToFile("  sete %%al")
				printlnToFile("  setnp %%dl")
				printlnToFile("  and %%dl, %%al")
			} else if node.Kind == ND_NE {
				printlnToFile("  setne %%al")
				printlnToFile("  setp %%dl")
				printlnToFile("  or %%dl, %%al")
			} else if node.Kind == ND_LT {
				printlnToFile("  seta %%al")
			} else {
				printlnToFile("  setae %%al")
			}

			printlnToFile("  and $1, %%al")
			printlnToFile("  movzb %%al, %%rax")
			return
		}

		errorTok(node.Tok, "invalid expression")
	} else if node.Lhs.Ty.Kind == TY_LDOUBLE {
		genExpr(node.Lhs)
		genExpr(node.Rhs)

		switch node.Kind {
		case ND_ADD:
			printlnToFile("  faddp")
			return
		case ND_SUB:
			printlnToFile("  fsubrp")
			return
		case ND_MUL:
			printlnToFile("  fmulp")
			return
		case ND_DIV:
			printlnToFile("  fdivrp")
			return
		case ND_EQ, ND_NE, ND_LT, ND_LE:
			printlnToFile("  fcomip")
			printlnToFile("  fstp %%st(0)")

			if node.Kind == ND_EQ {
				printlnToFile("  sete %%al")
			} else if node.Kind == ND_NE {
				printlnToFile("  setne %%al")
			} else if node.Kind == ND_LT {
				printlnToFile("  seta %%al")
			} else {
				printlnToFile("  setae %%al")
			}

			printlnToFile("  movzb %%al, %%rax")
			return
		}

		errorTok(node.Tok, "invalid expression")
	}

	genExpr(node.Rhs)
	pushTmp()
	genExpr(node.Lhs)
	popTmp("%rdi")

	ax := "%eax"
	di := "%edi"
	dx := "%edx"

	if node.Lhs.Ty.Size == 8 || node.Lhs.Ty.Base != nil {
		ax = "%rax"
		di = "%rdi"
		dx = "%rdx"
	}

	switch node.Kind {
	case ND_ADD:
		printlnToFile("  add %s, %s", di, ax)
		return
	case ND_SUB:
		printlnToFile("  sub %s, %s", di, ax)
		return
	case ND_MUL:
		printlnToFile("  imul %s, %s", di, ax)
		return
	case ND_DIV, ND_MOD:
		if node.Ty.IsUnsigned {
			printlnToFile("  mov $0, %s", dx)
			printlnToFile("  div %s", di)
		} else {
			if node.Lhs.Ty.Size == 8 {
				printlnToFile("  cqo")
			} else {
				printlnToFile("  cdq")
			}
			printlnToFile("  idiv %s", di)
		}

		if node.Kind == ND_MOD {
			printlnToFile("  mov %%rdx, %%rax")
		}
		return
	case ND_BITAND:
		printlnToFile("  and %s, %s", di, ax)
		return
	case ND_BITOR:
		printlnToFile("  or %s, %s", di, ax)
		return
	case ND_BITXOR:
		printlnToFile("  xor %s, %s", di, ax)
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		printlnToFile("  cmp %s, %s", di, ax)

		if node.Kind == ND_EQ {
			printlnToFile("  sete %%al")
		} else if node.Kind == ND_NE {
			printlnToFile("  setne %%al")
		} else if node.Kind == ND_LT {
			if node.Lhs.Ty.IsUnsigned {
				printlnToFile("  setb %%al")
			} else {
				printlnToFile("  setl %%al")
			}
		} else if node.Kind == ND_LE {
			if node.Lhs.Ty.IsUnsigned {
				printlnToFile("  setbe %%al")
			} else {
				printlnToFile("  setle %%al")
			}
		}

		printlnToFile("  movzb %%al, %%rax")
		return
	case ND_SHL:
		printlnToFile("  mov %%rdi, %%rcx")
		printlnToFile("  shl %%cl, %s", ax)
		return
	case ND_SHR:
		printlnToFile("  mov %%rdi, %%rcx")
		if node.Lhs.Ty.IsUnsigned {
			printlnToFile("  shr %%cl, %s", ax)
		} else {
			printlnToFile("  sar %%cl, %s", ax)
		}
		return
	}

	errorTok(node.Tok, "invalid expression")
}

func genStmt(node *AstNode) {
	if opt_g {
		printLoc(node.Tok)
	}

	switch node.Kind {
	case ND_IF:
		c := count()
		genExpr(node.Cond)
		cmpZero(node.Cond.Ty)
		printlnToFile("  je  .L.else.%d", c)
		genStmt(node.Then)
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.else.%d:", c)
		if node.Else != nil {
			genStmt(node.Else)
		}
		printlnToFile(".L.end.%d:", c)
		return
	case ND_FOR:
		c := count()
		if node.Init != nil {
			genStmt(node.Init)
		}
		printlnToFile(".L.begin.%d:", c)
		if node.Cond != nil {
			genExpr(node.Cond)
			cmpZero(node.Cond.Ty)
			printlnToFile("  je %s", node.BreakLabel)
		}
		genStmt(node.Then)
		printlnToFile("%s:", node.ContinueLabel)
		if node.Inc != nil {
			genExpr(node.Inc)
		}
		printlnToFile("  jmp .L.begin.%d", c)
		printlnToFile("%s:", node.BreakLabel)
		dealloc_vla(node)
		return
	case ND_DO:
		c := count()
		printlnToFile(".L.begin.%d:", c)
		genStmt(node.Then)
		printlnToFile("%s:", node.ContinueLabel)
		genExpr(node.Cond)
		cmpZero(node.Cond.Ty)
		printlnToFile("  jne .L.begin.%d", c)
		printlnToFile("%s:", node.BreakLabel)
		return
	case ND_SWITCH:
		genExpr(node.Cond)

		for n := node.CaseNext; n != nil; n = n.CaseNext {
			ax := "%eax"
			di := "%edi"
			dx := "%edx"
			if node.Cond.Ty.Size == 8 {
				ax = "%rax"
				di = "%rdi"
				dx = "%rdx"
			}

			if n.Begin == n.End {
				printlnToFile("  mov $%d, %s", n.Begin, dx)
				printlnToFile("  cmp %s, %s", dx, ax)
				printlnToFile("  je %s", n.Label)
				continue
			}

			// [GNU] Case ranges
			printlnToFile("  mov %s, %s", ax, di)
			printlnToFile("  mov $%d, %s", n.Begin, dx)
			printlnToFile("  sub %s, %s", dx, di)
			printlnToFile("  mov $%d, %s", n.End-n.Begin, dx)
			printlnToFile("  cmp %s, %s", dx, di)
			printlnToFile("  jbe %s", n.Label)
		}

		if node.DefaultCase != nil {
			printlnToFile("  jmp %s", node.DefaultCase.Label)
		}

		printlnToFile("  jmp %s", node.BreakLabel)
		genStmt(node.Then)
		printlnToFile("%s:", node.BreakLabel)
		return
	case ND_CASE:
		printlnToFile("%s:", node.Label)
		if node.Lhs != nil {
			genStmt(node.Lhs)
		}
		return
	case ND_BLOCK:
		for n := node.Body; n != nil; n = n.Next {
			genStmt(n)
		}
		dealloc_vla(node)
		return
	case ND_GOTO:
		dealloc_vla(node)
		printlnToFile("  jmp %s", node.UniqueLabel)
		return
	case ND_LABEL:
		printlnToFile("%s:", node.UniqueLabel)
		if node.Lhs != nil {
			genStmt(node.Lhs)
		}
		return
	case ND_GOTO_EXPR:
		genExpr(node.Lhs)
		printlnToFile("  jmp *%%rax")
		return
	case ND_RETURN:
		if node.Lhs != nil {
			genExpr(node.Lhs)

			ty := node.Lhs.Ty
			if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
				if ty.Size <= 16 {
					copyStructReg()
				} else {
					copyStructMem()
				}
			}
		}
		printlnToFile("  jmp .L.return.%s", currentFn.Name)
		return
	case ND_EXPR_STMT:
		genExpr(node.Lhs)
		return
	case ND_ASM:
		printlnToFile("  %s", node.AsmStr)
		return
	case ND_VA_START:
		genExpr(node.Lhs)
		fn := currentFn
		printlnToFile("  movl $%d, (%%rax)", fn.VaGpOffset)
		printlnToFile("  movl $%d, 4(%%rax)", fn.VaFpOffset)
		printlnToFile("  lea %d(%%rbp), %%rdx", fn.VaStOffset)
		printlnToFile("  movq %%rdx, 8(%%rax)")
		printlnToFile("  lea %d(%s), %%rdx", fn.VaArea.Offset, fn.VaArea.Pointer)
		printlnToFile("  movq %%rdx, 16(%%rax)")
		return
	case ND_VA_COPY:
		genExpr(node.Lhs)
		pushTmp()
		genExpr(node.Rhs)
		popTmp("%rdi")

		printlnToFile("  movq (%%rax), %%rdx")
		printlnToFile("  movq %%rdx, (%%rdi)")
		printlnToFile("  movq 8(%%rax), %%rdx")
		printlnToFile("  movq %%rdx, 8(%%rdi)")
		printlnToFile("  movq 16(%%rax), %%rdx")
		printlnToFile("  movq %%rdx, 16(%%rdi)")
		return
	}

	errorTok(node.Tok, "invalid statement")
}

func getLovalVarAlign(sc *Scope, align int64) int64 {
	for v := sc.Locals; v != nil; v = v.Next {
		if v.Offset != 0 {
			continue
		}
		align = int64(math.Max(float64(align), float64(v.Align)))
	}

	for sub := sc.Children; sub != nil; sub = sub.SiblingNext {
		subMax := getLovalVarAlign(sub, align)
		align = int64(math.Max(float64(align), float64(subMax)))
	}

	return align
}

// Assign offsets to local variables.
func assignLocalVariableOffsets(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.Next {
		if !fn.IsFunction || !fn.IsDefinition {
			continue
		}

		if fn.LargeRtn != nil {
			fn.LargeRtn.ParamNext = fn.Ty.ParamList
			fn.Ty.ParamList = fn.LargeRtn
		}

		// If a function has many parameters, some parameters are
		// inevitably passed by stack rather than by register.
		// The first passed-by-stack parameter resides at RBP+16.
		var top int64 = 16

		callingConvention(fn.Ty.ParamList, 0, nil, nil, nil)

		// Assign offsets to pass-by-stack parameters.
		for v := fn.Ty.ParamList; v != nil; v = v.ParamNext {
			if !v.PassByStack {
				continue
			}

			v.Offset = v.StackOffset + top
			v.Pointer = "%rbp"
		}

		fn.StackAlign = getLovalVarAlign(fn.Ty.Scopes, 16)

		lvarPtr := "%rbp"
		if fn.StackAlign > 16 {
			lvarPtr = "%rbx"
		}

		fn.LocalVarStackSize = int64(assignLocalVariableOffsets2(fn.Ty.Scopes, 0, lvarPtr))
	}
}

func assignLocalVariableOffsets2(sc *Scope, bottom int, ptr string) int {
	for v := sc.Locals; v != nil; v = v.Next {
		if v.Offset != 0 {
			continue
		}

		// AMD64 System V ABI has a special alignment rule for an array of
		// length at least 16 bytes. We need to align such array to at least
		// 16-byte boundaries. See p.14 of
		// https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
		align := v.Align
		if v.Ty.Kind == TY_ARRAY && v.Ty.Size >= 16 {
			align = int64(math.Max(16, float64(v.Align)))
		}

		bottom += int(v.Ty.Size)
		bottom = int(alignTo(int64(bottom), align))
		v.Offset = -int64(bottom)
		v.Pointer = ptr
	}

	maxDepth := bottom
	for sub := sc.Children; sub != nil; sub = sub.SiblingNext {
		subDepth := assignLocalVariableOffsets2(sub, bottom, ptr)
		if !DontReuseStack {
			maxDepth = int(math.Max(float64(maxDepth), float64(subDepth)))
		} else {
			maxDepth = subDepth
			bottom = maxDepth
		}
	}

	return maxDepth
}

func builtin_alloca(node *AstNode) {
	// Shift the temporary area by %rdi.
	printlnToFile("  sub %%rax, %%rsp")
	// Align frame pointer
	align := int64(16)
	if node.Value > 16 {
		align = node.Value
	}
	printlnToFile("  and $-%d, %%rsp", align)
	if node.Variable != nil {
		printlnToFile("  mov %%rsp, %d(%s)", node.Variable.Offset, node.Variable.Pointer)
		printlnToFile("  mov %%rsp, %d(%s)", node.TopVLA.Offset, node.Variable.Pointer)
	}
	printlnToFile("  mov %%rsp, %%rax")
}

func dealloc_vla(node *AstNode) {
	if currentFn.VlaBase == nil || node.TopVLA == node.TargetVLA {
		return
	}

	vla := currentFn.VlaBase
	if node.TargetVLA != nil {
		vla = node.TargetVLA
	}
	printlnToFile("  mov %d(%s), %%rsp", vla.Offset, vla.Pointer)
}

func emitData(prog *Obj) {
	for v := prog; v != nil; v = v.Next {
		if v.IsFunction || !v.IsDefinition {
			continue
		}

		if v.IsStatic {
			printlnToFile("  .local %s", v.Name)
		} else {
			printlnToFile("  .globl %s", v.Name)
		}

		align := v.Align
		if v.Ty.Kind == TY_ARRAY && v.Ty.Size >= 16 {
			align = int64(math.Max(16, float64(v.Align)))
		}

		// Common symbol
		if opt_fcommon && v.IsTentative {
			printlnToFile("  .comm %s, %d, %d", v.Name, v.Ty.Size, align)
			continue
		}

		// .data or .tdata
		if v.InitData != nil {
			if v.IsTls && opt_data_sections {
				printlnToFile("  .section .tdata.%s,\"awT\",@progbits", v.Name)
			} else if v.IsTls {
				printlnToFile("  .section .tdata,\"awT\",@progbits")
			} else if opt_data_sections {
				printlnToFile("  .section .data.%s,\"aw\",@progbits", v.Name)
			} else {
				printlnToFile("  .data")
			}

			printlnToFile("  .type %s, @object", v.Name)
			printlnToFile("  .size %s, %d", v.Name, v.Ty.Size)
			printlnToFile("  .align %d", align)
			printlnToFile("%s:", v.Name)

			rel := v.Rel
			pos := 0
			for pos < int(v.Ty.Size) {
				if rel != nil && rel.Offset == int64(pos) {
					printlnToFile("  .quad %s%+d", *rel.Label, rel.Addend)
					rel = rel.Next
					pos += 8
				} else {
					printlnToFile("  .byte %d", v.InitData[pos])
					pos += 1
				}
			}

			continue
		}

		// .bss or .tbss
		if v.IsTls && opt_data_sections {
			printlnToFile("  .section .tbss.%s,\"awT\",@nobits", v.Name)
		} else if v.IsTls {
			printlnToFile("  .section .tbss,\"awT\",@nobits")
		} else if opt_data_sections {
			printlnToFile("  .section .bss.%s,\"aw\",@nobits", v.Name)
		} else {
			printlnToFile("  .bss")
		}

		printlnToFile("  .align %d", align)
		printlnToFile("%s:", v.Name)
		printlnToFile("  .zero %d", v.Ty.Size)
	}
}

func emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.Next {
		if !fn.IsFunction || !fn.IsDefinition {
			continue
		}

		// No code is emitted for "static inline" functions
		// if no one is referencing them.
		if !fn.IsLive {
			continue
		}

		if fn.IsStatic {
			printlnToFile("  .local %s", fn.Name)
		} else {
			printlnToFile("  .globl %s", fn.Name)
		}

		if opt_func_sections {
			printlnToFile("  .section .text.%s,\"ax\",@progbits", fn.Name)
		} else {
			printlnToFile("  .text")
		}
		printlnToFile("  .type %s, @function", fn.Name)
		printlnToFile("%s:", fn.Name)
		currentFn = fn
		tmpStack.Bottom = int(fn.LocalVarStackSize)

		useRBX := fn.StackAlign > 16
		LocalVarPointer = "%rbp"
		if useRBX {
			LocalVarPointer = "%rbx"
		}

		// Prologue
		printlnToFile("  push %%rbp")
		printlnToFile("  mov %%rsp, %%rbp")
		if useRBX {
			printlnToFile("  push %%rbx")
			printlnToFile("  mov %%rsp, %%rbx")
			printlnToFile("  and $-%d, %%rbx", fn.StackAlign)
			printlnToFile("  mov %%rbx, %%rsp")
		}
		reservedPos := len(*cgOutputFile)
		printlnToFile("PLACEHOLDER")
		if fn.VlaBase != nil {
			printlnToFile("  mov %%rsp, %d(%s)", fn.VlaBase.Offset, fn.VlaBase.Pointer)
		}

		// Save arg registers if function is variadic
		if fn.VaArea != nil {
			gp := 0
			fp := 0
			stack := callingConvention(fn.Ty.ParamList, 0, &gp, &fp, nil)
			fn.VaGpOffset = int64(gp) * 8
			fn.VaFpOffset = int64(fp)*16 + 48
			fn.VaStOffset = int64(stack) + 16

			off := fn.VaArea.Offset
			ptr := LocalVarPointer

			//__reg_save_area__
			printlnToFile("  movq %%rdi, %d(%s)", off, ptr)
			printlnToFile("  movq %%rsi, %d(%s)", off+8, ptr)
			printlnToFile("  movq %%rdx, %d(%s)", off+16, ptr)
			printlnToFile("  movq %%rcx, %d(%s)", off+24, ptr)
			printlnToFile("  movq %%r8, %d(%s)", off+32, ptr)
			printlnToFile("  movq %%r9, %d(%s)", off+40, ptr)
			printlnToFile("  test %%al, %%al")
			printlnToFile("  je 1f")
			printlnToFile("  movsd %%xmm0, %d(%s)", off+48, ptr)
			printlnToFile("  movsd %%xmm1, %d(%s)", off+64, ptr)
			printlnToFile("  movsd %%xmm2, %d(%s)", off+80, ptr)
			printlnToFile("  movsd %%xmm3, %d(%s)", off+96, ptr)
			printlnToFile("  movsd %%xmm4, %d(%s)", off+112, ptr)
			printlnToFile("  movsd %%xmm5, %d(%s)", off+128, ptr)
			printlnToFile("  movsd %%xmm6, %d(%s)", off+144, ptr)
			printlnToFile("  movsd %%xmm7, %d(%s)", off+160, ptr)
			printlnToFile("1:")
		}

		// Save passed-by-register arguments to the stack
		gp := 0
		fp := 0
		for v := fn.Ty.ParamList; v != nil; v = v.ParamNext {
			if v.PassByStack {
				continue
			}

			ty := v.Ty
			switch ty.Kind {
			case TY_STRUCT, TY_UNION:
				if ty.Size > 16 {
					panic("ty.Size > 16")
				}
				if ty.hasFloatNumber1() {
					storeFp(fp, int64(math.Min(8, float64(ty.Size))), v.Offset, v.Pointer)
					fp++
				} else {
					storeGp(gp, int64(math.Min(8, float64(ty.Size))), v.Offset, v.Pointer)
					gp++
				}
				if ty.Size > 8 {
					if ty.hasFloatNumber2() {
						storeFp(fp, ty.Size-8, v.Offset+8, v.Pointer)
						fp++
					} else {
						storeGp(gp, ty.Size-8, v.Offset+8, v.Pointer)
						gp++
					}
				}
			case TY_FLOAT, TY_DOUBLE:
				storeFp(fp, ty.Size, v.Offset, v.Pointer)
				fp++
			case TY_LDOUBLE:
				panic("unreachable")
			default:
				storeGp(gp, ty.Size, v.Offset, v.Pointer)
				gp++
			}
		}

		// Emit code
		genStmt(fn.Body)

		if tmpStack.Depth != 0 {
			panic("tmp stack depth is not zero")
		}

		inst := fmt.Sprintf("  sub $%d, %%rsp", alignTo(int64(tmpStack.Bottom), 16))
		(*cgOutputFile)[reservedPos] = inst

		// [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
		// a special rule for the main function. Reaching the end of the
		// main function is equivalent to returning 0, even though the
		// behavior is undefined for the other functions.
		if fn.Name == "main" {
			printlnToFile("  mov $0, %%rax")
		}

		// Epilogue
		printlnToFile(".L.return.%s:", fn.Name)
		if useRBX {
			printlnToFile("  mov -8(%%rbp), %%rbx")
		}
		printlnToFile("  mov %%rbp, %%rsp")
		printlnToFile("  pop %%rbp")
		printlnToFile("  ret")
	}
}

func codegen(prog *Obj, out *[]string) {
	cgOutputFile = out

	if opt_g {
		files := getInputFiles()
		for _, f := range files {
			printlnToFile("  .file %d \"%s\"", f.FileNo, f.Name)
		}
	}

	assignLocalVariableOffsets(prog)
	emitData(prog)
	emitText(prog)
	printlnToFile("  .section  .note.GNU-stack,\"\",@progbits")
}
