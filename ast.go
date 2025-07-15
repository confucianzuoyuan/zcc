package main

import "fmt"

type AstNodeKind uint8

// AST Type
const (
	ND_NULL_EXPR AstNodeKind = iota // Do nothing
	ND_ADD                          // +
	ND_SUB                          // -
	ND_MUL                          // *
	ND_DIV                          // /
	ND_POS                          // unary +
	ND_NEG                          // unary -
	ND_EQ                           // ==
	ND_NE                           // !=
	ND_LT                           // <
	ND_LE                           // <=
	ND_GT                           // >
	ND_GE                           // >=
	ND_ASSIGN                       // =
	ND_COND                         // Ternary conditional (?:)
	ND_COMMA                        // ,
	ND_MEMBER                       // . (struct member access)
	ND_ADDR                         // unary &
	ND_DEREF                        // unary *
	ND_NOT                          // !
	ND_BITNOT                       // ~
	ND_MOD                          // %
	ND_BITAND                       // &
	ND_BITOR                        // |
	ND_BITXOR                       // ^
	ND_SHL                          // <<
	ND_SHR                          // >>
	ND_SAR                          // arithmetic >>
	ND_LOGAND                       // &&
	ND_LOGOR                        // ||
	ND_RETURN                       // "return"
	ND_IF                           // "if"
	ND_FOR                          // "for" or "while"
	ND_DO                           // "do"
	ND_SWITCH                       // "switch"
	ND_CASE                         // "case"
	ND_BLOCK                        // { ... }
	ND_GOTO                         // "goto"
	ND_GOTO_EXPR                    // "goto" labels-as-values
	ND_LABEL                        // Labeled statement
	ND_LABEL_VAL                    // [GNU] Labels-as-values
	ND_FUNCALL                      // Function call
	ND_EXPR_STMT                    // Expression statement
	ND_STMT_EXPR                    // Statement expression
	ND_VAR                          // Variable
	ND_NUM                          // Integer
	ND_CAST                         // Type cast
	ND_MEMZERO                      // Zero-clear a stack variable
	ND_ASM                          // "asm"
	ND_CAS                          // Atomic compare-and-swap
	ND_EXCH                         // Atomic exchange
	ND_VA_START                     // "va_start"
	ND_VA_COPY                      // "va_copy"
	ND_VA_ARG                       // "va_arg"
	ND_CHAIN                        // ND_COMMA without array-to-pointer conversion
)

func (k AstNodeKind) String() string {
	switch k {
	case ND_NULL_EXPR:
		return "ND_NULL_EXPR"
	case ND_ADD:
		return "ND_ADD"
	case ND_SUB:
		return "ND_SUB"
	case ND_MUL:
		return "ND_MUL"
	case ND_DIV:
		return "ND_DIV"
	case ND_POS:
		return "ND_POS"
	case ND_NEG:
		return "ND_NEG"
	case ND_EQ:
		return "ND_EQ"
	case ND_NE:
		return "ND_NE"
	case ND_LT:
		return "ND_LT"
	case ND_LE:
		return "ND_LE"
	case ND_GT:
		return "ND_GT"
	case ND_GE:
		return "ND_GE"
	case ND_ASSIGN:
		return "ND_ASSIGN"
	case ND_COND:
		return "ND_COND"
	case ND_COMMA:
		return "ND_COMMA"
	case ND_MEMBER:
		return "ND_MEMBER"
	case ND_ADDR:
		return "ND_ADDR"
	case ND_DEREF:
		return "ND_DEREF"
	case ND_NOT:
		return "ND_NOT"
	case ND_BITNOT:
		return "ND_BITNOT"
	case ND_MOD:
		return "ND_MOD"
	case ND_BITAND:
		return "ND_BITAND"
	case ND_BITOR:
		return "ND_BITOR"
	case ND_BITXOR:
		return "ND_BITXOR"
	case ND_SHL:
		return "ND_SHL"
	case ND_SHR:
		return "ND_SHR"
	case ND_SAR:
		return "ND_SAR"
	case ND_LOGAND:
		return "ND_LOGAND"
	case ND_LOGOR:
		return "ND_LOGOR"
	case ND_RETURN:
		return "ND_RETURN"
	case ND_IF:
		return "ND_IF"
	case ND_FOR:
		return "ND_FOR"
	case ND_DO:
		return "ND_DO"
	case ND_SWITCH:
		return "ND_SWITCH"
	case ND_CASE:
		return "ND_CASE"
	case ND_BLOCK:
		return "ND_BLOCK"
	case ND_GOTO:
		return "ND_GOTO"
	case ND_GOTO_EXPR:
		return "ND_GOTO_EXPR"
	case ND_LABEL:
		return "ND_LABEL"
	case ND_LABEL_VAL:
		return "ND_LABEL_VAL"
	case ND_FUNCALL:
		return "ND_FUNCALL"
	case ND_EXPR_STMT:
		return "ND_EXPR_STMT"
	case ND_STMT_EXPR:
		return "ND_STMT_EXPR"
	case ND_VAR:
		return "ND_VAR"
	case ND_NUM:
		return "ND_NUM"
	case ND_CAST:
		return "ND_CAST"
	case ND_MEMZERO:
		return "ND_MEMZERO"
	case ND_ASM:
		return "ND_ASM"
	case ND_CAS:
		return "ND_CAS"
	case ND_EXCH:
		return "ND_EXCH"
	case ND_VA_START:
		return "ND_VA_START"
	case ND_VA_COPY:
		return "ND_VA_COPY"
	case ND_VA_ARG:
		return "ND_VA_ARG"
	case ND_CHAIN:
		return "ND_CHAIN"
	default:
		return "AstNodeKind(" + fmt.Sprint(uint8(k)) + ")"
	}
}

type AstNode struct {
	Kind AstNodeKind // Node Kind
	Next *AstNode
	Ty   *CType
	Tok  *Token // Representative token

	Lhs *AstNode
	Rhs *AstNode

	Cond *AstNode
	Then *AstNode
	Else *AstNode
	Init *AstNode
	Inc  *AstNode

	// "break" and "continue" labels
	BreakLabel    string
	ContinueLabel string

	// Block or statement expression
	Body *AstNode

	// Struct member access
	Member *Member

	// Function call
	Args         *Obj
	ReturnBuffer *Obj
	ArgsExpr     *AstNode

	// Goto or labeled statement, or labels-as-values
	Label       string
	UniqueLabel string
	GotoNext    *AstNode

	// Switch
	CaseNext    *AstNode
	DefaultCase *AstNode

	// Case
	Begin int64
	End   int64

	TargetVLA *Obj
	TopVLA    *Obj

	// "asm" string literal
	AsmStr string

	// Atomic compare-and-swap
	CasAddr *AstNode
	CasOld  *AstNode
	CasNew  *AstNode

	// Variable
	Variable *Obj

	// Numeric literal
	Value      int64
	FloatValue FloatConst
}

func (node *AstNode) String() string {
	s := ""
	if node.Kind == ND_VAR {
		s = s + "var: " + B2S((*node.Tok.File.Contents)[node.Tok.Location:node.Tok.Location+node.Tok.Length]) + "\n"
	}
	return s
}

// Struct member
type Member struct {
	Next     *Member
	Ty       *CType
	Name     *Token
	Index    int
	AltAlign int64
	Offset   int64 // Offset from the beginning of the struct

	// Bitfield
	IsBitfield bool
	BitOffset  int64
	BitWidth   int64
}
