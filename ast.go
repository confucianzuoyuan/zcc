package main

type AstNodeKind uint8

// AST Type
const (
	ND_NULL_EXPR AstNodeKind = iota // Do nothing
	ND_ADD                          // +
	ND_SUB                          // -
	ND_MUL                          // *
	ND_DIV                          // /
	ND_NEG                          // unary -
	ND_EQ                           // ==
	ND_NE                           // !=
	ND_LT                           // <
	ND_LE                           // <=
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
	ND_VLA_PTR                      // VLA designator
	ND_NUM                          // Integer
	ND_CAST                         // Type cast
	ND_MEMZERO                      // Zero-clear a stack variable
	ND_ASM                          // "asm"
	ND_CAS                          // Atomic compare-and-swap
	ND_EXCH                         // Atomic exchange
)

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
	FloatValue float64
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
	Next   *Member
	Ty     *CType
	Name   *Token
	Index  int
	Align  int64
	Offset int64 // Offset from the beginning of the struct

	// Bitfield
	IsBitfield bool
	BitOffset  int64
	BitWidth   int64
}
