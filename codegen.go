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
func alignTo(n int, align int) int {
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
	}

	errorTok(node.Tok, "not an lvalue")
}

// Load a value from where %rax is pointing to.
func load(ty *CType) {
	if ty.Kind == TY_ARRAY {
		// If it is an array, do not attempt to load a value to the
		// register because in general we can't load an entire array to a
		// register. As a result, the result of an evaluation of an array
		// becomes not the array itself but the address of the array.
		// This is where "array is automatically converted to a pointer to
		// the first element of the array in C" occurs.
		return
	}

	if ty.Size == 1 {
		printlnToFile("  movsbq (%%rax), %%rax")
	} else {
		printlnToFile("  mov (%%rax), %%rax")
	}
}

// Store %rax to an address that the stack top is pointing to.
func store(ty *CType) {
	pop("%rdi")

	if ty.Size == 1 {
		printlnToFile("  mov %%al, (%%rdi)")
	} else {
		printlnToFile("  mov %%rax, (%%rdi)")
	}
}

// Generate code for a given node.
func genExpr(node *AstNode) {
	printlnToFile("  .loc 1 %d", node.Tok.LineNo)

	switch node.Kind {
	case ND_NUM:
		printlnToFile("  mov $%d, %%rax", node.Value)
		return
	case ND_NEG:
		genExpr(node.Lhs)
		printlnToFile("  neg %%rax")
		return
	case ND_VAR:
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

	switch node.Kind {
	case ND_ADD:
		printlnToFile("  add %%rdi, %%rax")
		return
	case ND_SUB:
		printlnToFile("  sub %%rdi, %%rax")
		return
	case ND_MUL:
		printlnToFile("  imul %%rdi, %%rax")
		return
	case ND_DIV:
		printlnToFile("  cqo")
		printlnToFile("  idiv %%rdi")
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		printlnToFile("  cmp %%rdi, %%rax")

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
			printlnToFile("  je  .L.end.%d", c)
		}
		genStmt(node.Then)
		if node.Inc != nil {
			genExpr(node.Inc)
		}
		printlnToFile("  jmp .L.begin.%d", c)
		printlnToFile(".L.end.%d:", c)
		return
	case ND_BLOCK:
		for n := node.Body; n != nil; n = n.Next {
			genStmt(n)
		}
		return
	case ND_RETURN:
		genExpr(node.Lhs)
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

		offset := 0
		for v := fn.Locals; v != nil; v = v.Next {
			offset += v.Ty.Size
			v.Offset = -offset
		}
		fn.StackSize = alignTo(offset, 16)
	}
}

func emitData(prog *Obj) {
	for v := prog; v != nil; v = v.Next {
		if v.IsFunction {
			continue
		}

		printlnToFile("  .data")
		printlnToFile("  .globl %s", v.Name)
		printlnToFile("%s:", v.Name)

		if v.InitData != nil {
			for i := 0; i < v.Ty.Size; i += 1 {
				printlnToFile("  .byte %d", v.InitData[i])
			}
		} else {
			printlnToFile("  .zero %d", v.Ty.Size)
		}
	}
}

func emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.Next {
		if !fn.IsFunction {
			continue
		}

		printlnToFile("  .globl %s", fn.Name)
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
			if v.Ty.Size == 1 {
				printlnToFile("  mov %s, %d(%%rbp)", argreg8[i], v.Offset)
				i += 1
			} else {
				printlnToFile("  mov %s, %d(%%rbp)", argreg64[i], v.Offset)
				i += 1
			}
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
