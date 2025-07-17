package main

// Variable or function
type Obj struct {
	Next    *Obj
	Name    string // Variable name
	Ty      *CType // Type
	IsLocal bool   // local or global/function
	Align   int64  // alignment

	// Local variable
	Offset        int64
	Pointer       string
	ParamNext     *Obj
	VlaNext       *Obj
	PassByStack   bool
	StackOffset   int64
	ParamPromoted *Obj

	// Global variable or function
	IsFunction   bool
	IsDefinition bool
	IsStatic     bool

	// Global variable
	IsTentative bool
	IsTls       bool
	InitData    []int8
	Rel         *Relocation

	// constexpr variable
	ConstExprData []int8

	// Function
	IsInline   bool
	DeallocVLA bool
	Body       *AstNode

	// Static inline function
	IsLive bool
	IsRoot bool
	Refs   []string
}

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
type Relocation struct {
	Next   *Relocation
	Offset int64
	Label  *string
	Addend int64
}

// Represents a block scope.
type Scope struct {
	Parent      *Scope
	Children    *Scope
	SiblingNext *Scope

	Locals      *Obj
	IsTemporary bool
	// C has two block scopes; one is for variables/typedefs and
	// the other is for struct/union/enum tags.
	Vars map[string]*VarScope
	Tags map[string]*CType
}
