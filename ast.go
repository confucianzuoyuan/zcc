package main

type AstNodeKind uint8

// AST Type
const (
	ND_ADD       AstNodeKind = iota // +
	ND_SUB                          // -
	ND_MUL                          // *
	ND_DIV                          // /
	ND_NEG                          // unary -
	ND_EQ                           // ==
	ND_NE                           // !=
	ND_LT                           // <
	ND_LE                           // <=
	ND_ASSIGN                       // =
	ND_COMMA                        // ,
	ND_MEMBER                       // . (struct member access)
	ND_ADDR                         // unary &
	ND_DEREF                        // unary *
	ND_RETURN                       // "return"
	ND_IF                           // "if"
	ND_FOR                          // "for" or "while"
	ND_BLOCK                        // { ... }
	ND_FUNCALL                      // Function call
	ND_EXPR_STMT                    // Expression statement
	ND_STMT_EXPR                    // Statement expression
	ND_VAR                          // Variable
	ND_NUM                          // Integer
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

	// Block or statement expression
	Body *AstNode

	// Struct member access
	Member *Member

	// Function call
	FuncName string
	Args     *AstNode

	Variable *Obj // Used if kind == ND_VAR
	Value    int  // Used if kind == ND_NUM
}

func (node *AstNode) String() string {
	s := ""
	if node.Kind == ND_VAR {
		s = s + "var: " + string((*currentInput)[node.Tok.Location:node.Tok.Location+node.Tok.Length]) + "\n"
	}
	return s
}

// Struct member
type Member struct {
	Next   *Member
	Ty     *CType
	Name   *Token
	Offset int // Offset from the beginning of the struct
}
