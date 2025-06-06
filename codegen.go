package main

import (
	"fmt"
	"io"
)

var outputFile io.Writer
var depth int
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

var i int = 1

var currentFn *Obj

func printlnToFile(fmtStr string, args ...interface{}) {
	fmt.Fprintf(outputFile, fmtStr, args...)
	// 输出换行符
	fmt.Fprintln(outputFile)
}

func push() {
	printlnToFile("  push %%rax")
	depth += 1
}

func pop(arg string) {
	printlnToFile("  pop %s", arg)
	depth -= 1
}

func count() int {
	i += 1
	return i - 1
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
		if node.Variable.IsLocal {
			// Local variable
			printlnToFile("  lea %d(%%rbp), %%rax", node.Variable.Offset)
		} else {
			// Global variable
			printlnToFile("  lea %s(%%rip), %%rax", node.Variable.Name)
		}
		return
	case ND_DEREF:
		genExpr(node.Lhs)
		return
	case ND_COMMA:
		genExpr(node.Lhs)
		genAddr(node.Rhs)
		return
	case ND_MEMBER:
		genAddr(node.Lhs)
		printlnToFile("  add $%d, %%rax", node.Member.Offset)
		return
	}

	errorTok(node.Tok, "not an lvalue")
}

// Load a value from where %rax is pointing to.
func load(ty *CType) {
	if ty.Kind == TY_ARRAY || ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
		// If it is an array, do not attempt to load a value to the
		// register because in general we can't load an entire array to a
		// register. As a result, the result of an evaluation of an array
		// becomes not the array itself but the address of the array.
		// This is where "array is automatically converted to a pointer to
		// the first element of the array in C" occurs.
		return
	}

	// When we load a char or a short value to a register, we always
	// extend them to the size of int, so we can assume the lower half of
	// a register always contains a valid value. The upper half of a
	// register for char, short and int may contain garbage. When we load
	// a long value to a register, it simply occupies the entire register.
	if ty.Size == 1 {
		printlnToFile("  movsbl (%%rax), %%eax")
	} else if ty.Size == 2 {
		printlnToFile("  movswl (%%rax), %%eax")
	} else if ty.Size == 4 {
		printlnToFile("  movsxd (%%rax), %%rax")
	} else {
		printlnToFile("  mov (%%rax), %%rax")
	}
}

// Store %rax to an address that the stack top is pointing to.
func store(ty *CType) {
	pop("%rdi")

	if ty.Kind == TY_STRUCT || ty.Kind == TY_UNION {
		for i := int64(0); i < ty.Size; i += 1 {
			printlnToFile("  mov %d(%%rax), %%r8b", i)
			printlnToFile("  mov %%r8b, %d(%%rdi)", i)
		}
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
)

func getTypeId(ty *CType) int {
	switch ty.Kind {
	case TY_CHAR:
		return I8
	case TY_SHORT:
		return I16
	case TY_INT:
		return I32
	}
	return I64
}

// The table for type casts
const I32I8 = "movsbl %al, %eax"
const I32I16 = "movswl %ax, %eax"
const I32I64 = "movsxd %eax, %rax"

var CastTable = [][]string{
	// I8
	{"", "", "", I32I64},
	// I16
	{I32I8, "", "", I32I64},
	// I32
	{I32I8, I32I16, "", I32I64},
	// I64
	{I32I8, I32I16, "", ""},
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

func storeGp(r int, offset int64, sz int64) {
	switch sz {
	case 1:
		printlnToFile("  mov %s, %d(%%rbp)", argreg8[r], offset)
		return
	case 2:
		printlnToFile("  mov %s, %d(%%rbp)", argreg16[r], offset)
		return
	case 4:
		printlnToFile("  mov %s, %d(%%rbp)", argreg32[r], offset)
		return
	case 8:
		printlnToFile("  mov %s, %d(%%rbp)", argreg64[r], offset)
		return
	}

	panic("unreachable in store_gp")
}

// Generate code for a given node.
func genExpr(node *AstNode) {
	printlnToFile("  .loc 1 %d", node.Tok.LineNo)

	switch node.Kind {
	case ND_NULL_EXPR:
		return
	case ND_NUM:
		printlnToFile("  mov $%d, %%rax", node.Value)
		return
	case ND_NEG:
		genExpr(node.Lhs)
		printlnToFile("  neg %%rax")
		return
	case ND_VAR, ND_MEMBER:
		genAddr(node)
		load(node.Ty)
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
		store(node.Ty)
		return
	case ND_STMT_EXPR:
		for n := node.Body; n != nil; n = n.Next {
			genStmt(n)
		}
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
		printlnToFile("  lea %d(%%rbp), %%rdi", node.Variable.Offset)
		printlnToFile("  xor %%al, %%al")
		printlnToFile("  rep stosb")
		return
	case ND_COND:
		c := count()
		genExpr(node.Cond)
		printlnToFile("  cmp $0, %%rax")
		printlnToFile("  je .L.else.%d", c)
		genExpr(node.Then)
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.else.%d:", c)
		genExpr(node.Else)
		printlnToFile(".L.end.%d:", c)
		return
	case ND_NOT:
		genExpr(node.Lhs)
		printlnToFile("  cmp $0, %%rax")
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
		printlnToFile("  cmp $0, %%rax")
		printlnToFile("  je  .L.false.%d", c)
		genExpr(node.Rhs)
		printlnToFile("  cmp $0, %%rax")
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
		printlnToFile("  cmp $0, %%rax")
		printlnToFile("  jne .L.true.%d", c)
		genExpr(node.Rhs)
		printlnToFile("  cmp $0, %%rax")
		printlnToFile("  jne .L.true.%d", c)
		printlnToFile("  mov $0, %%rax")
		printlnToFile("  jmp .L.end.%d", c)
		printlnToFile(".L.true.%d:", c)
		printlnToFile("  mov $1, %%rax")
		printlnToFile(".L.end.%d:", c)
		return
	case ND_FUNCALL:
		nargs := 0
		for arg := node.Args; arg != nil; arg = arg.Next {
			genExpr(arg)
			push()
			nargs += 1
		}

		for i := nargs - 1; i >= 0; i -= 1 {
			pop(argreg64[i])
		}

		printlnToFile("  mov $0, %%rax")
		printlnToFile("  call %s", node.FuncName)
		return
	}

	genExpr(node.Rhs)
	push()
	genExpr(node.Lhs)
	pop("%rdi")

	ax := "%eax"
	di := "%edi"

	if node.Lhs.Ty.Kind == TY_LONG || node.Lhs.Ty.Base != nil {
		ax = "%rax"
		di = "%rdi"
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
		if node.Lhs.Ty.Size == 8 {
			printlnToFile("  cqo")
		} else {
			printlnToFile("  cdq")
		}
		printlnToFile("  idiv %s", di)

		if node.Kind == ND_MOD {
			printlnToFile("  mov %%rdx, %%rax")
		}
		return
	case ND_BITAND:
		printlnToFile("  and %%rdi, %%rax")
		return
	case ND_BITOR:
		printlnToFile("  or %%rdi, %%rax")
		return
	case ND_BITXOR:
		printlnToFile("  xor %%rdi, %%rax")
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		printlnToFile("  cmp %s, %s", di, ax)

		if node.Kind == ND_EQ {
			printlnToFile("  sete %%al")
		} else if node.Kind == ND_NE {
			printlnToFile("  setne %%al")
		} else if node.Kind == ND_LT {
			printlnToFile("  setl %%al")
		} else if node.Kind == ND_LE {
			printlnToFile("  setle %%al")
		}

		printlnToFile("  movzb %%al, %%rax")
		return
	case ND_SHL:
		printlnToFile("  mov %%rdi, %%rcx")
		printlnToFile("  shl %%cl, %s", ax)
		return
	case ND_SHR:
		printlnToFile("  mov %%rdi, %%rcx")
		if node.Ty.Size == 8 {
			printlnToFile("  sar %%cl, %s", ax)
		} else {
			printlnToFile("  sar %%cl, %s", ax)
		}
		return
	}

	errorTok(node.Tok, "invalid expression")
}

func genStmt(node *AstNode) {
	printlnToFile("  .loc 1 %d", node.Tok.LineNo)

	switch node.Kind {
	case ND_IF:
		c := count()
		genExpr(node.Cond)
		printlnToFile("  cmp $0, %%rax")
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
			printlnToFile("  cmp $0, %%rax")
			printlnToFile("  je %s", node.BreakLabel)
		}
		genStmt(node.Then)
		printlnToFile("%s:", node.ContinueLabel)
		if node.Inc != nil {
			genExpr(node.Inc)
		}
		printlnToFile("  jmp .L.begin.%d", c)
		printlnToFile("%s:", node.BreakLabel)
		return
	case ND_DO:
		c := count()
		printlnToFile(".L.begin.%d:", c)
		genStmt(node.Then)
		printlnToFile("%s:", node.ContinueLabel)
		genExpr(node.Cond)
		printlnToFile("  cmp $0, %%rax")
		printlnToFile("  jne .L.begin.%d", c)
		printlnToFile("%s:", node.BreakLabel)
		return
	case ND_SWITCH:
		genExpr(node.Cond)

		for n := node.CaseNext; n != nil; n = n.CaseNext {
			reg := "%eax"
			if node.Cond.Ty.Size == 8 {
				reg = "%rax"
			}
			printlnToFile("  cmp $%d, %s", n.Value, reg)
			printlnToFile("  je %s", n.Label)
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
		genStmt(node.Lhs)
		return
	case ND_BLOCK:
		for n := node.Body; n != nil; n = n.Next {
			genStmt(n)
		}
		return
	case ND_GOTO:
		printlnToFile("  jmp %s", node.UniqueLabel)
		return
	case ND_LABEL:
		printlnToFile("%s:", node.UniqueLabel)
		genStmt(node.Lhs)
		return
	case ND_RETURN:
		if node.Lhs != nil {
			genExpr(node.Lhs)
		}
		printlnToFile("  jmp .L.return.%s", currentFn.Name)
		return
	case ND_EXPR_STMT:
		genExpr(node.Lhs)
		return
	}

	errorTok(node.Tok, "invalid statement")
}

// Assign offsets to local variables.
func assignLocalVariableOffsets(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.Next {
		if !fn.IsFunction {
			continue
		}

		offset := int64(0)
		for v := fn.Locals; v != nil; v = v.Next {
			offset += v.Ty.Size
			offset = alignTo(offset, v.Align)
			v.Offset = -offset
		}
		fn.StackSize = alignTo(offset, 16)
	}
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
		printlnToFile("  .align %d", v.Align)

		if v.InitData != nil {
			printlnToFile("  .data")
			printlnToFile("%s:", v.Name)

			rel := v.Rel
			pos := 0
			for pos < int(v.Ty.Size) {
				if rel != nil && rel.Offset == int64(pos) {
					printlnToFile("  .quad %s%+d", rel.Label, rel.Addend)
					rel = rel.Next
					pos += 8
				} else {
					printlnToFile("  .byte %d", v.InitData[pos])
					pos += 1
				}
			}

			continue
		}

		printlnToFile("  .bss")
		printlnToFile("%s:", v.Name)
		printlnToFile("  .zero %d", v.Ty.Size)
	}
}

func emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.Next {
		if !fn.IsFunction || !fn.IsDefinition {
			continue
		}

		if fn.IsStatic {
			printlnToFile("  .local %s", fn.Name)
		} else {
			printlnToFile("  .globl %s", fn.Name)
		}

		printlnToFile("  .text")
		printlnToFile("%s:", fn.Name)
		currentFn = fn

		// Prologue
		printlnToFile("  push %%rbp")
		printlnToFile("  mov %%rsp, %%rbp")
		printlnToFile("  sub $%d, %%rsp", fn.StackSize)

		// Save passed-by-register arguments to the stack
		i := 0
		for v := fn.Params; v != nil; v = v.Next {
			storeGp(i, v.Offset, v.Ty.Size)
			i += 1
		}

		// Emit code
		genStmt(fn.Body)
		if depth != 0 {
			panic("depth is not zero.")
		}

		// Epilogue
		printlnToFile(".L.return.%s:", fn.Name)
		printlnToFile("  mov %%rbp, %%rsp")
		printlnToFile("  pop %%rbp")
		printlnToFile("  ret")
	}
}

func codegen(prog *Obj, out io.Writer) {
	outputFile = out

	assignLocalVariableOffsets(prog)
	emitData(prog)
	emitText(prog)
}
