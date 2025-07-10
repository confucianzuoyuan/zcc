package main

import (
	"fmt"
	"math"
)

const FP_MAX = 8
const GP_MAX = 6
const GP_SLOTS = 6
const FP_SLOTS = 6

type SlotKind int

const (
	SL_GP SlotKind = iota
	SL_FP
	SL_ST
)

type Slot struct {
	Kind     SlotKind
	GpDepth  int64
	FpDepth  int64
	StDepth  int64
	StOffset int64
	Location int64
}

var cgOutputFile *[]string
var argreg8 = [6]string{"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"}
var argreg16 = [6]string{"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"}
var argreg32 = [6]string{"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"}
var argreg64 = [6]string{"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}
var tmpreg32 = [6]string{"%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d"}
var tmpreg64 = [6]string{"%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11"}

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

func reservedLine() int64 {
	reservedPos := len(*cgOutputFile)
	printlnToFile("PLACEHOLDER")
	return int64(reservedPos)
}

func instructionLine(fmtStr string, reservedPos int, args ...any) {
	code := fmt.Sprintf(fmtStr, args...)
	(*cgOutputFile)[reservedPos] = code
}

type TmpStack struct {
	Data     []Slot
	Capacity int
	Depth    int
	Bottom   int
}

var tmpStack = TmpStack{}

func saveTmpRegs() {
	for i := 0; i < tmpStack.Depth; i++ {
		tmpStack.Data[i].Kind = SL_ST
	}
}

func pushTmpStack(kind SlotKind) {
	if tmpStack.Depth == tmpStack.Capacity {
		tmpStack.Capacity += 4
		for range 4 {
			tmpStack.Data = append(tmpStack.Data, Slot{})
		}
	}

	loc := reservedLine()
	sl := Slot{Kind: kind, Location: loc}
	tmpStack.Data[tmpStack.Depth] = sl
	tmpStack.Depth++
}

func popTmpStack() *Slot {
	tmpStack.Depth--
	if tmpStack.Depth < 0 {
		panic("tmpStack.Depth < 0")
	}

	sl := &tmpStack.Data[tmpStack.Depth]
	if (sl.Kind == SL_GP && sl.GpDepth >= GP_SLOTS) || (sl.Kind == SL_FP && sl.FpDepth >= FP_SLOTS) {
		sl.Kind = SL_ST
	}

	if tmpStack.Depth > 0 {
		sl2 := &tmpStack.Data[tmpStack.Depth-1]
		sl2.GpDepth = int64(math.Max(float64(sl2.GpDepth), float64(sl.GpDepth+int64(boolToInt(sl.Kind == SL_GP)))))
		sl2.FpDepth = int64(math.Max(float64(sl2.FpDepth), float64(sl.FpDepth+int64(boolToInt(sl.Kind == SL_FP)))))
		sl2.StDepth = int64(math.Max(float64(sl2.StDepth), float64(sl.StDepth+int64(boolToInt(sl.Kind == SL_ST)))))
	}

	if sl.Kind == SL_ST {
		if DontReuseStack {
			tmpStack.Bottom += 8
			sl.StOffset = -int64(tmpStack.Bottom)
		} else {
			bottom := currentFn.LocalVarStackSize + (sl.StDepth+1)*8
			tmpStack.Bottom = int(math.Max(float64(tmpStack.Bottom), float64(bottom)))
			sl.StOffset = -bottom
		}
	}

	return sl
}

func genMemZero(dofs int, dptr string, sz int) {
	printlnToFile("  xor %%eax, %%eax")
	for i := 0; i < sz; {
		rem := sz - i
		p2 := 1
		if rem >= 8 {
			p2 = 8
		} else if rem >= 4 {
			p2 = 4
		} else if rem >= 2 {
			p2 = 2
		}
		printlnToFile("  mov %s, %d(%s)", regAX(p2), i+dofs, dptr)
		i += p2
	}
}

func push() {
	pushTmpStack(SL_GP)
}

func pop2(sl *Slot, isR64 bool, arg string) {
	ax := "%eax"
	if isR64 {
		ax = "%rax"
	}
	if sl.Kind == SL_GP {
		reg := tmpreg32[sl.GpDepth]
		if isR64 {
			reg = tmpreg64[sl.GpDepth]
		}
		instructionLine("  mov %s, %s", int(sl.Location), ax, reg)
		printlnToFile("  mov %s, %s", reg, arg)
		return
	}
	instructionLine("  mov %s, %d(%s)", int(sl.Location), ax, sl.StOffset, LocalVarPointer)
	printlnToFile("  mov %d(%s), %s", sl.StOffset, LocalVarPointer, arg)
}

func pop(arg string) {
	sl := popTmpStack()
	pop2(sl, true, arg)
}

func reqGP(regtbl []string, i int) string {
	if i+1 > GP_SLOTS {
		panic("i+1 > GP_SLOTS")
	}

	if tmpStack.Depth > 0 {
		sl := &tmpStack.Data[tmpStack.Depth-1]
		sl.GpDepth = int64(math.Max(float64(sl.GpDepth), float64(i+1)))
	}
	return regtbl[i]
}

func popInReg2(isR64 bool, fallbackReg string) string {
	sl := popTmpStack()

	if sl.Kind == SL_GP {
		ax := "%eax"
		reg := tmpreg32[sl.GpDepth]
		if isR64 {
			ax = "%rax"
			reg = tmpreg64[sl.GpDepth]
		}
		instructionLine("  mov %s, %s", int(sl.Location), ax, reg)
		return reg
	}
	pop2(sl, isR64, fallbackReg)
	return fallbackReg
}

func popInReg(fallbackReg string) string {
	return popInReg2(true, fallbackReg)
}

func pushF() {
	pushTmpStack(SL_FP)
}

func popFloatInReg(isXMM64 bool) int {
	sl := popTmpStack()
	mv := "movss"
	if isXMM64 {
		mv = "movsd"
	}

	if sl.Kind == SL_FP {
		instructionLine("  %s %%xmm0, %%xmm%d", int(sl.Location), mv, sl.FpDepth+2)
		return int(sl.FpDepth) + 2
	}
	instructionLine("  %s %%xmm0, %d(%s)", int(sl.Location), mv, sl.StOffset, LocalVarPointer)
	printlnToFile("  %s %d(%s), %%xmm1", mv, sl.StOffset, LocalVarPointer)
	return 1
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
		case TY_STRUCT, TY_UNION, TY_LDOUBLE, TY_FLOAT, TY_DOUBLE:
			genMemCopy(int(v.Offset), v.Pointer, int(v.StackOffset), "%rsp", int(v.Ty.Size))
			continue
		}

		loadExtendInt(v.Ty, int(v.Offset), v.Pointer, regOpAX(v.Ty))
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

	printlnToFile("  mov %%rax, %%rcx")

	if ty.hasFloatNumber1() {
		if !(ty.Size == 4 || 8 <= ty.Size) {
			panic("")
		}
		if ty.Size == 4 {
			printlnToFile("  movss (%%rcx), %%xmm0")
		} else {
			printlnToFile("  movsd (%%rcx), %%xmm0")
		}
		fp += 1
	} else {
		printlnToFile("  mov $0, %%rax")
		for i := int64(math.Min(8, float64(ty.Size))) - 1; i >= 0; i -= 1 {
			printlnToFile("  shl $8, %%rax")
			printlnToFile("  mov %d(%%rcx), %%al", i)
		}
		gp += 1
	}

	if ty.Size > 8 {
		if ty.hasFloatNumber2() {
			if !(ty.Size == 12 || ty.Size == 16) {
				panic("type size must be 12 or 16")
			}

			if ty.Size == 12 {
				printlnToFile("  movss 8(%%rcx), %%xmm%d", fp)
			} else {
				printlnToFile("  movsd 8(%%rcx), %%xmm%d", fp)
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
				printlnToFile("  mov %d(%%rcx), %s", i, reg1)
			}
		}
	}
}

func copyStructMem() {
	ty := currentFn.Ty.ReturnType
	v := currentFn.Ty.ParamList

	printlnToFile("  mov %d(%s), %%rcx", v.Offset, v.Pointer)
	genMemCopy(0, "%rax", 0, "%rcx", int(ty.Size))
	printlnToFile("  mov %%rcx, %%rax")
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

func regOpAX(ty *CType) string {
	switch ty.Size {
	case 1, 2, 4:
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

func genVaArgRegCopy(ty *CType, v *Obj) {
	regClass := []bool{!ty.hasFloatNumber1(), !ty.hasFloatNumber2()}

	gpInc := boolToInt(regClass[0]) + boolToInt(regClass[1])
	if gpInc > 0 {
		printlnToFile("  cmpl $%d, (%%rax)", 48-gpInc*8)
		printlnToFile("  ja 1f")
	}
	fpInc := boolToInt(!regClass[0]) + boolToInt(!regClass[1])
	printlnToFile("  cmpl $%d, 4(%%rax)", 176-fpInc*16)
	printlnToFile("  ja 1f")

	for i := int64(0); i < 2; i++ {
		if regClass[i] {
			printlnToFile("  movl (%%rax), %%ecx")   // gp_offset
			printlnToFile("  addq 16(%%rax), %%rcx") // reg_save_area
			printlnToFile("  addq $8, (%%rax)")
		} else {
			printlnToFile("  movl 4(%%rax), %%ecx")  // fp_offset
			printlnToFile("  addq 16(%%rax), %%rcx") // reg_save_area
			printlnToFile("  addq $16, 4(%%rax)")
		}
		genMemCopy(0, "%rcx", int(i*8+v.Offset), v.Pointer, int(math.Min(float64(ty.Size-i*8), 8)))
	}
	printlnToFile("  lea %d(%s), %%rdx", v.Offset, v.Pointer)
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
				saveTmpRegs()
				printlnToFile("  data16 lea \"%s\"@tlsgd(%%rip), %%rdi", node.Variable.Name)
				printlnToFile("  .value 0x6666")
				printlnToFile("  rex64")
				printlnToFile("  call __tls_get_addr@PLT")
				return
			}

			// Function or global variable
			printlnToFile("  mov \"%s\"@GOTPCREL(%%rip), %%rax", node.Variable.Name)
			return
		}

		// Thread-local variable
		if node.Variable.IsTls {
			printlnToFile("  mov %%fs:0, %%rax")
			printlnToFile("  add $\"%s\"@tpoff, %%rax", node.Variable.Name)
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
				printlnToFile("  lea \"%s\"(%%rip), %%rax", node.Variable.Name)
			} else {
				printlnToFile("  mov \"%s\"@GOTPCREL(%%rip), %%rax", node.Variable.Name)
			}
			return
		}

		// Global variable
		printlnToFile("  lea \"%s\"(%%rip), %%rax", node.Variable.Name)
		return
	case ND_DEREF:
		genExpr(node.Lhs)
		return
	case ND_CHAIN, ND_COMMA:
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
		case ND_ASSIGN, ND_COND, ND_STMT_EXPR, ND_VA_ARG:
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
	if ty.isScalar() {
		load2(ty, 0, "%rax")
		return
	}
	if ty.Kind == TY_ARRAY || ty.Kind == TY_STRUCT || ty.Kind == TY_UNION || ty.Kind == TY_FUNC || ty.Kind == TY_VLA {
		// If it is an array, do not attempt to load a value to the
		// register because in general we can't load an entire array to a
		// register. As a result, the result of an evaluation of an array
		// becomes not the array itself but the address of the array.
		// This is where "array is automatically converted to a pointer to
		// the first element of the array in C" occurs.
		return
	}

	panic("internal error")
}

func store2(ty *CType, dofs int, dptr string) {
	if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION || ty.Kind == TY_ARRAY {
		genMemCopy(0, "%rax", dofs, dptr, int(ty.Size))
		return
	}

	if ty.Kind == TY_FLOAT {
		printlnToFile("  movss %%xmm0, %d(%s)", dofs, dptr)
		return
	}

	if ty.Kind == TY_DOUBLE {
		printlnToFile("  movsd %%xmm0, %d(%s)", dofs, dptr)
		return
	}

	if ty.Kind == TY_LDOUBLE {
		printlnToFile("  fstpt %d(%s)", dofs, dptr)
		return
	}

	printlnToFile("  mov %s, %d(%s)", regAX(int(ty.Size)), dofs, dptr)
}

// Store %rax to an address that the stack top is pointing to.
func store(ty *CType) {
	reg := popInReg("%rcx")
	store2(ty, 0, reg)
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

func genCmpSetx(kind AstNodeKind, isUnsigned bool) {
	ins := ""
	switch kind {
	case ND_EQ:
		ins = "sete"
	case ND_NE:
		ins = "setne"
	case ND_LT:
		ins = "setl"
		if isUnsigned {
			ins = "setb"
		}
	case ND_LE:
		ins = "setle"
		if isUnsigned {
			ins = "setbe"
		}
	case ND_GT:
		ins = "setg"
		if isUnsigned {
			ins = "seta"
		}
	case ND_GE:
		ins = "setge"
		if isUnsigned {
			ins = "setae"
		}
	default:
		panic("internal error")
	}
	printlnToFile("  %s %%al", ins)
	printlnToFile("  movzbl %%al, %%eax")
}

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
const I32F80 = "push %rax; fildl (%rsp); pop %rax"

const U32F32 = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0"
const U32I64 = "mov %eax, %eax"
const U32F64 = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0"
const U32F80 = "mov %eax, %eax; push %rax; fildll (%rsp); pop %rax"

const I64F32 = "cvtsi2ssq %rax, %xmm0"
const I64F64 = "cvtsi2sdq %rax, %xmm0"
const I64F80 = "push %rax; fildll (%rsp); pop %rax"

const U64F32 = `test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2ss %rax,%xmm0; jmp 2f; 
  1: mov %rax,%rdx; and $1,%eax; pxor %xmm0,%xmm0; shr %rdx; 
  or %rax,%rdx; cvtsi2ss %rdx,%xmm0; addss %xmm0,%xmm0; 2:`
const U64F64 = `test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; 
                1: mov %rax,%rdx; and $1,%eax; pxor %xmm0,%xmm0; shr %rdx; 
                or %rax,%rdx; cvtsi2sd %rdx,%xmm0; addsd %xmm0,%xmm0; 2:`
const U64F80 = `push %rax; fildq (%rsp); test %rax, %rax; jns 1f;
                mov $1602224128, %eax; mov %eax, 4(%rsp); fadds 4(%rsp); 1:; pop %rax`

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
const F32F80 = "sub $8, %rsp; movss %xmm0, (%rsp); flds (%rsp); add $8, %rsp"

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
const F64F80 = "sub $8, %rsp; movsd %xmm0, (%rsp); fldl (%rsp); add $8, %rsp"

const FROM_F80_1 = `sub $24, %rsp; fnstcw 14(%rsp); movzwl 14(%rsp), %eax; or $12, %ah; mov %ax, 12(%rsp); fldcw 12(%rsp); `
const FROM_F80_2 = " (%rsp); fldcw 14(%rsp); "
const FROM_F80_3 = "; add $24, %rsp"

const F80I8 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movsbl (%rsp), %eax" + FROM_F80_3
const F80U8 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl (%rsp), %eax" + FROM_F80_3
const F80I16 = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl (%rsp), %eax" + FROM_F80_3
const F80U16 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "movswl (%rsp), %eax" + FROM_F80_3
const F80I32 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov (%rsp), %eax" + FROM_F80_3
const F80U32 = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov (%rsp), %eax" + FROM_F80_3
const F80I64 = FROM_F80_1 + "fistpq" + FROM_F80_2 + "mov (%rsp), %rax" + FROM_F80_3
const F80U64 = `sub $16, %rsp; movl $0x5f000000, 12(%rsp); flds 12(%rsp); fucomi %st(1), %st; setbe %al;
  fldz; fcmovbe %st(1), %st; fstp %st(1); fsubrp %st, %st(1); fnstcw 4(%rsp);
  movzwl 4(%rsp), %ecx; orl $3072, %ecx; movw %cx, 6(%rsp); fldcw 6(%rsp);
  fistpll 8(%rsp); fldcw 4(%rsp); shlq $63, %rax; xorq 8(%rsp), %rax; add $16, %rsp`
const F80F32 = "sub $8, %rsp; fstps (%rsp); movss (%rsp), %xmm0; add $8, %rsp"
const F80F64 = "sub $8, %rsp; fstpl (%rsp); movsd (%rsp), %xmm0; add $8, %rsp"

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
		genCmpSetx(ND_NE, false)
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

func genMemCopy(sofs int, sptr string, dofs int, dptr string, sz int) {
	for i := 0; i < sz; {
		rem := sz - i
		if rem >= 16 {
			printlnToFile("  movups %d(%s), %%xmm0", i+sofs, sptr)
			printlnToFile("  movups %%xmm0, %d(%s)", i+dofs, dptr)
			i += 16
			continue
		}
		p2 := 1
		if rem >= 8 {
			p2 = 8
		} else if rem >= 4 {
			p2 = 4
		} else if rem >= 2 {
			p2 = 2
		}
		printlnToFile("  mov %d(%s), %s", i+sofs, sptr, regDX(p2))
		printlnToFile("  mov %s, %d(%s)", regDX(p2), i+dofs, dptr)
		i += p2
	}
}

func inImmRange(val int64) bool {
	return val == int64(int32(val))
}

func loadValue(ty *CType, val int64) {
	if val == 0 {
		printlnToFile("  xor %%eax, %%eax")
		return
	}
	if ty.Size <= 4 && inImmRange(val) {
		printlnToFile("  movl $%d, %%eax", val)
		return
	} else if ty.Size > 4 && uint64(val) <= math.MaxUint32 {
		printlnToFile("  movl $%d, %%eax", val)
		return
	} else {
		printlnToFile("  mov $%d, %%rax", val)
	}
}

func loadFloatValue(ty *CType, fval float64) {
	if ty.Kind == TY_FLOAT {
		var posZ float32 = +0.0
		fv := float32(fval)
		if math.Float32bits(posZ) == math.Float32bits(fv) {
			printlnToFile("  xorps %%xmm0, %%xmm0")
			return
		}
	}
	if ty.Kind == TY_DOUBLE {
		var posZ float64 = +0.0
		dv := fval
		if math.Float64bits(posZ) == math.Float64bits(dv) {
			printlnToFile("  xorps %%xmm0, %%xmm0")
			return
		}
	}
	if ty.Kind == TY_LDOUBLE {
		var posZ float64 = +0.0
		if math.Float64bits(posZ) == math.Float64bits(fval) {
			printlnToFile("  fldz")
			return
		}
		negZ := float64(-0.0)
		if math.Float64bits(negZ) == math.Float64bits(fval) {
			printlnToFile("  fldz")
			printlnToFile("  fchs")
			return
		}
		if fval == 1 {
			printlnToFile("  fld1")
			return
		}
		if fval == -1 {
			printlnToFile("  fld1")
			printlnToFile("  fchs")
			return
		}
	}
	switch ty.Kind {
	case TY_FLOAT:
		u := math.Float32bits(float32(fval))
		printlnToFile("  mov $%d, %%eax  # float %f", u, fval)
		printlnToFile("  movq %%rax, %%xmm0")
	case TY_DOUBLE:
		u := math.Float64bits(fval)
		printlnToFile("  mov $%d, %%rax  # double %f", u, fval)
		printlnToFile("  movq %%rax, %%xmm0")
	case TY_LDOUBLE:
		u := NewFromFloat64(fval)
		printlnToFile("  mov $%d, %%rax  # long double %f", u.m, fval)
		printlnToFile("  mov %%rax, -16(%%rsp)")
		printlnToFile("  mov $%d, %%rax", u.se)
		printlnToFile("  mov %%rax, -8(%%rsp)")
		printlnToFile("  fldt -16(%%rsp)")
		return
	}
}

func genExprOpt(node *AstNode) bool {
	kind := node.Kind
	ty := node.Ty
	lhs := node.Lhs
	rhs := node.Rhs

	if node.isLVar() && ty.isScalar() {
		load2(ty, int(node.Variable.Offset), node.Variable.Pointer)
		return true
	}

	if kind == ND_ASSIGN && lhs.isLVar() {
		genExpr(rhs)
		store2(lhs.Variable.Ty, int(lhs.Variable.Offset), lhs.Variable.Pointer)
		return true
	}

	if kind != ND_NUM {
		ival := int64(0)
		if ty.isInteger() && node.isConstExpr(&ival) {
			loadValue(ty, ival)
			return true
		}

		fval := float64(0)
		if ty.isFloat() && node.isConstDouble(&fval) {
			loadFloatValue(ty, fval)
			return true
		}
	}

	return false
}

func (node *AstNode) isLVar() bool {
	return node.Kind == ND_VAR && node.Variable.IsLocal
}

func load2(ty *CType, sofs int, sptr string) {
	switch ty.Kind {
	case TY_FLOAT:
		printlnToFile("  movss %d(%s), %%xmm0", sofs, sptr)
		return
	case TY_DOUBLE:
		printlnToFile("  movsd %d(%s), %%xmm0", sofs, sptr)
		return
	case TY_LDOUBLE:
		printlnToFile("  fldt %d(%s)", sofs, sptr)
		return
	}

	loadExtendInt(ty, sofs, sptr, regOpAX(ty))
}

// Generate code for a given node.
func genExpr(node *AstNode) {
	if opt_g {
		printLoc(node.Tok)
	}

	if opt_optimize && genExprOpt(node) {
		return
	}

	switch node.Kind {
	case ND_NULL_EXPR:
		return
	case ND_LABEL_VAL:
		printlnToFile("  lea %s(%%rip), %%rax", node.UniqueLabel)
		return
	case ND_VA_ARG:
		genExpr(node.Lhs)

		ty := node.Ty.Base
		if ty.Size <= 16 {
			if ty.vaArgNeedCopy() {
				// Structs with FP member are split into 8-byte chunks in the
				// reg save area, we reconstruct the layout with a local copy.
				genVaArgRegCopy(ty, node.Variable)
			} else if ty.hasFloatNumber1() {
				printlnToFile("  cmpl $%d, 4(%%rax)", 160)
				printlnToFile("  ja 1f")
				printlnToFile("  movl 4(%%rax), %%edx")  // fp_offset
				printlnToFile("  addq 16(%%rax), %%rdx") // reg_save_area
				printlnToFile("  addq $16, 4(%%rax)")
			} else {
				gpInc := 1
				if ty.Size > 8 {
					gpInc = 2
				}
				printlnToFile("  cmpl $%d, (%%rax)", 48-gpInc*8)
				printlnToFile("  ja 1f")
				printlnToFile("  movl (%%rax), %%edx")   // gp_offset
				printlnToFile("  addq 16(%%rax), %%rdx") // reg_save_area
				printlnToFile("  addq $%d, (%%rax)", gpInc*8)
			}

			printlnToFile("  jmp 2f")
			printlnToFile("1:")
		}

		printlnToFile("  movq 8(%%rax), %%rdx") // overflow_arg_area
		if ty.Align <= 8 {
			printlnToFile("  addq $%d, 8(%%rax)", alignTo(ty.Size, 8))
		} else {
			printlnToFile("  addq $%d, %%rdx", ty.Align-1)
			printlnToFile("  andq $-%d, %%rdx", ty.Align)
			printlnToFile("  lea %d(%%rdx), %%rcx", alignTo(ty.Size, 8))
			printlnToFile("  movq %%rcx, 8(%%rax)")
		}

		if ty.Size <= 16 {
			printlnToFile("2:")
		}
		printlnToFile("  mov %%rdx, %%rax")
		return
	case ND_EXCH:
		genExpr(node.Lhs)
		push()
		genExpr(node.Rhs)
		reg := popInReg("%rcx")

		sz := int(node.Lhs.Ty.Base.Size)
		printlnToFile("  xchg %s, (%s)", regAX(sz), reg)
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
		push()
		genExpr(node.Rhs)
		reg := popInReg("%rcx")
		genMemCopy(0, "%rax", 0, reg, 24)
		return
	case ND_CAS:
		genExpr(node.CasAddr)
		push()
		genExpr(node.CasOld)
		push()
		genExpr(node.CasNew)
		sz := int(node.CasAddr.Ty.Base.Size)
		printlnToFile("  mov %s, %s", regAX(sz), regDX(sz))
		pop("%rax") // old
		pop("%rcx") // addr

		r0 := reqGP(tmpreg64[:], 0)
		printlnToFile("  mov %%rax, %s", r0)
		load(node.CasOld.Ty.Base)

		printlnToFile("  lock cmpxchg %s, (%%rcx)", regDX(sz))
		printlnToFile("  sete %%cl")
		printlnToFile("  je 1f")
		printlnToFile("  mov %s, (%s)", regAX(sz), r0)
		printlnToFile("1:")
		printlnToFile("  movzbl %%cl, %%eax")
		return
	case ND_NUM:
		if node.Ty.isFloat() {
			loadFloatValue(node.Ty, node.FloatValue)
			return
		}
		loadValue(node.Ty, node.Value)
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
		push()
		genExpr(node.Rhs)
		if node.Lhs.isBitField() {
			// If the lhs is a bitfield, we need to read the current value
			// from memory and merge it with a new value.
			mem := node.Lhs.Member
			printlnToFile("  mov $%d, %%rdx", (1<<mem.BitWidth)-1)
			printlnToFile("  and %%rdx, %%rax")
			printlnToFile("  mov %%rax, %%rcx")

			pop("%rax")
			push()
			load(mem.Ty)

			mask := ((1 << mem.BitWidth) - 1) << mem.BitOffset
			printlnToFile("  mov $%d, %%rdx", ^mask)
			printlnToFile("  and %%rdx, %%rax")

			printlnToFile("  mov %%rcx, %%rdx")
			printlnToFile("  shl $%d, %%rdx", mem.BitOffset)
			printlnToFile("  or %%rdx, %%rax")
			store(node.Ty)
			printlnToFile("  mov %%rcx, %%rax")

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
	case ND_CHAIN, ND_COMMA:
		genExpr(node.Lhs)
		genExpr(node.Rhs)
		return
	case ND_CAST:
		genExpr(node.Lhs)
		cast(node.Lhs.Ty, node.Ty)
		return
	case ND_MEMZERO:
		genMemZero(int(node.Variable.Offset), node.Variable.Pointer, int(node.Variable.Ty.Size))
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
	case ND_SHL, ND_SHR, ND_SAR:
		genExpr(node.Lhs)
		push()
		genExpr(node.Rhs)
		printlnToFile("  mov %%al, %%cl")

		ax := regOpAX(node.Ty)
		pop2(popTmpStack(), node.Ty.Size == 8, ax)

		switch node.Kind {
		case ND_SHL:
			printlnToFile("  shl %%cl, %s", ax)
		case ND_SHR:
			printlnToFile("  shr %%cl, %s", ax)
		case ND_SAR:
			printlnToFile("  sar %%cl, %s", ax)
		}
		return
	case ND_FUNCALL:
		if node.Lhs.Kind == ND_VAR && node.Lhs.Variable.Name == "alloca" {
			genExpr(node.ArgsExpr)
			builtin_alloca(node)
			return
		}

		genExpr(node.Lhs)
		push()

		if node.ArgsExpr != nil {
			genExpr(node.ArgsExpr)
		}

		pop("%r10")

		printlnToFile("  mov %%rsp, %%rax")
		push()

		saveTmpRegs()

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

		if node.Lhs.Ty.IsVariadic {
			printlnToFile("  mov $%d, %%eax", fpCount)
		}
		printlnToFile("  call *%%r10")

		pop("%rsp")

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
		genExpr(node.Lhs)
		pushF()
		genExpr(node.Rhs)

		isXMM64 := node.Lhs.Ty.Kind == TY_DOUBLE
		reg := popFloatInReg(isXMM64)
		sz := "ss"
		if isXMM64 {
			sz = "sd"
		}

		switch node.Kind {
		case ND_ADD:
			printlnToFile("  add%s %%xmm%d, %%xmm0", sz, reg)
			return
		case ND_SUB:
			printlnToFile("  sub%s %%xmm0, %%xmm%d", sz, reg)
			printlnToFile("  movaps %%xmm%d, %%xmm0", reg)
			return
		case ND_MUL:
			printlnToFile("  mul%s %%xmm%d, %%xmm0", sz, reg)
			return
		case ND_DIV:
			printlnToFile("  div%s %%xmm0, %%xmm%d", sz, reg)
			printlnToFile("  movaps %%xmm%d, %%xmm0", reg)
			return
		case ND_EQ, ND_NE, ND_LT, ND_LE, ND_GE, ND_GT:
			if node.Kind == ND_GT || node.Kind == ND_GE {
				printlnToFile("  ucomi%s %%xmm0, %%xmm%d", sz, reg)
			} else {
				printlnToFile("  ucomi%s %%xmm%d, %%xmm0", sz, reg)
			}

			if node.Kind == ND_EQ {
				printlnToFile("  sete %%al")
				printlnToFile("  setnp %%dl")
				printlnToFile("  and %%dl, %%al")
			} else if node.Kind == ND_NE {
				printlnToFile("  setne %%al")
				printlnToFile("  setp %%dl")
				printlnToFile("  or %%dl, %%al")
			} else if node.Kind == ND_LT || node.Kind == ND_GT {
				printlnToFile("  seta %%al")
			} else if node.Kind == ND_LE || node.Kind == ND_GE {
				printlnToFile("  setae %%al")
			}

			printlnToFile("  movzbl %%al, %%eax")
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
		case ND_EQ, ND_NE, ND_LT, ND_LE, ND_GT, ND_GE:
			if node.Kind == ND_GT || node.Kind == ND_GE {
				printlnToFile("  fxch %%st(1)")
			}

			printlnToFile("  fucomip")
			printlnToFile("  fstp %%st(0)")

			if node.Kind == ND_EQ {
				printlnToFile("  sete %%al")
				printlnToFile("  setnp %%dl")
				printlnToFile("  and %%dl, %%al")
			} else if node.Kind == ND_NE {
				printlnToFile("  setne %%al")
				printlnToFile("  setp %%dl")
				printlnToFile("  or %%dl, %%al")
			} else if node.Kind == ND_LT || node.Kind == ND_GT {
				printlnToFile("  seta %%al")
			} else if node.Kind == ND_LE || node.Kind == ND_GE {
				printlnToFile("  setae %%al")
			}

			printlnToFile("  movzbl %%al, %%eax")
			return
		}

		errorTok(node.Tok, "invalid expression")
	}

	genExpr(node.Lhs)
	push()
	genExpr(node.Rhs)

	isR64 := node.Lhs.Ty.Size == 8 || node.Lhs.Ty.Base != nil
	ax := "%eax"
	if isR64 {
		ax = "%rax"
	}
	cx := "%ecx"
	if isR64 {
		cx = "%rcx"
	}
	op := popInReg2(isR64, cx)

	switch node.Kind {
	case ND_ADD:
		printlnToFile("  add %s, %s", op, ax)
		return
	case ND_SUB:
		printlnToFile("  sub %s, %s", ax, op)
		printlnToFile("  mov %s, %s", op, ax)
		return
	case ND_MUL:
		printlnToFile("  imul %s, %s", op, ax)
		return
	case ND_DIV, ND_MOD:
		printlnToFile("  xchg %s, %s", op, ax)
		if node.Ty.IsUnsigned {
			printlnToFile("  xor %%edx, %%edx")
			printlnToFile("  div %s", op)
		} else {
			if node.Lhs.Ty.Size == 8 {
				printlnToFile("  cqo")
			} else {
				printlnToFile("  cdq")
			}
			printlnToFile("  idiv %s", op)
		}

		if node.Kind == ND_MOD {
			printlnToFile("  mov %%rdx, %%rax")
		}
		return
	case ND_BITAND:
		printlnToFile("  and %s, %s", op, ax)
		return
	case ND_BITOR:
		printlnToFile("  or %s, %s", op, ax)
		return
	case ND_BITXOR:
		printlnToFile("  xor %s, %s", op, ax)
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE, ND_GE, ND_GT:
		printlnToFile("  cmp %s, %s", ax, op)

		genCmpSetx(node.Kind, node.Lhs.Ty.IsUnsigned)
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

		ax := "%eax"
		cx := "%ecx"
		dx := "%edx"

		if node.Cond.Ty.Size == 8 {
			ax = "%rax"
			cx = "%rcx"
			dx = "%rdx"
		}

		for n := node.CaseNext; n != nil; n = n.CaseNext {
			if n.Begin == n.End {
				printlnToFile("  mov $%d, %s", n.Begin, dx)
				printlnToFile("  cmp %s, %s", dx, ax)
				printlnToFile("  je %s", n.Label)
				continue
			}

			// [GNU] Case ranges
			printlnToFile("  mov %s, %s", ax, cx)
			printlnToFile("  mov $%d, %s", n.Begin, dx)
			printlnToFile("  sub %s, %s", dx, cx)
			printlnToFile("  mov $%d, %s", n.End-n.Begin, dx)
			printlnToFile("  cmp %s, %s", dx, cx)
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
		printlnToFile("  jmp 9f")
		return
	case ND_EXPR_STMT:
		genExpr(node.Lhs)
		return
	case ND_ASM:
		printlnToFile("  %s", node.AsmStr)
		return
	}

	errorTok(node.Tok, "invalid statement")
}

func calcStackAlign(sc *Scope, align *int64) {
	for v := sc.Locals; v != nil; v = v.Next {
		if v.Offset != 0 {
			continue
		}
		*align = int64(math.Max(float64(*align), float64(v.Align)))
	}

	for sub := sc.Children; sub != nil; sub = sub.SiblingNext {
		calcStackAlign(sub, align)
	}
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

		stAlign := int64(16)
		calcStackAlign(fn.Ty.Scopes, &stAlign)
		fn.StackAlign = stAlign

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
		if DontReuseStack {
			maxDepth = subDepth
			bottom = subDepth
		} else {
			maxDepth = int(math.Max(float64(maxDepth), float64(subDepth)))
		}
	}

	return maxDepth
}

func builtin_alloca(node *AstNode) {
	// Shift the temporary area by %rax.
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
			printlnToFile("  .local \"%s\"", v.Name)
		} else {
			printlnToFile("  .globl \"%s\"", v.Name)
		}

		align := v.Align
		if v.Ty.Kind == TY_ARRAY && v.Ty.Size >= 16 {
			align = int64(math.Max(16, float64(v.Align)))
		}

		// Common symbol
		if opt_fcommon && v.IsTentative {
			printlnToFile("  .comm \"%s\", %d, %d", v.Name, v.Ty.Size, align)
			continue
		}

		// .data or .tdata
		if v.InitData != nil {
			if v.IsTls && opt_data_sections {
				printlnToFile("  .section .tdata.\"%s\",\"awT\",@progbits", v.Name)
			} else if v.IsTls {
				printlnToFile("  .section .tdata,\"awT\",@progbits")
			} else if opt_data_sections {
				printlnToFile("  .section .data.\"%s\",\"aw\",@progbits", v.Name)
			} else {
				printlnToFile("  .data")
			}

			printlnToFile("  .type \"%s\", @object", v.Name)
			printlnToFile("  .size \"%s\", %d", v.Name, v.Ty.Size)
			printlnToFile("  .align %d", align)
			printlnToFile("\"%s\":", v.Name)

			rel := v.Rel
			pos := 0
			for pos < int(v.Ty.Size) {
				if rel != nil && rel.Offset == int64(pos) {
					printlnToFile("  .quad \"%s\"%+d", *rel.Label, rel.Addend)
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
			printlnToFile("  .section .tbss.\"%s\",\"awT\",@nobits", v.Name)
		} else if v.IsTls {
			printlnToFile("  .section .tbss,\"awT\",@nobits")
		} else if opt_data_sections {
			printlnToFile("  .section .bss.\"%s\",\"aw\",@nobits", v.Name)
		} else {
			printlnToFile("  .bss")
		}

		printlnToFile("  .align %d", align)
		printlnToFile("\"%s\":", v.Name)
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
			printlnToFile("  .local \"%s\"", fn.Name)
		} else {
			printlnToFile("  .globl \"%s\"", fn.Name)
		}

		if opt_func_sections {
			printlnToFile("  .section .text.\"%s\",\"ax\",@progbits", fn.Name)
		} else {
			printlnToFile("  .text")
		}
		printlnToFile("  .type \"%s\", @function", fn.Name)
		printlnToFile("\"%s\":", fn.Name)
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

		stackAllocLocation := reservedLine()
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
			printlnToFile("  movups %%xmm0, %d(%s)", off+48, ptr)
			printlnToFile("  movups %%xmm1, %d(%s)", off+64, ptr)
			printlnToFile("  movups %%xmm2, %d(%s)", off+80, ptr)
			printlnToFile("  movups %%xmm3, %d(%s)", off+96, ptr)
			printlnToFile("  movups %%xmm4, %d(%s)", off+112, ptr)
			printlnToFile("  movups %%xmm5, %d(%s)", off+128, ptr)
			printlnToFile("  movups %%xmm6, %d(%s)", off+144, ptr)
			printlnToFile("  movups %%xmm7, %d(%s)", off+160, ptr)
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

		instructionLine("  sub $%d, %%rsp", int(stackAllocLocation), alignTo(int64(tmpStack.Bottom), 16))

		// [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
		// a special rule for the main function. Reaching the end of the
		// main function is equivalent to returning 0, even though the
		// behavior is undefined for the other functions.
		if fn.Name == "main" {
			printlnToFile("  mov $0, %%rax")
		}

		// Epilogue
		printlnToFile("9:")
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
