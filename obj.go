package main

// Variable or function
type Obj struct {
	Next    *Obj
	Name    string
	Ty      *CType
	IsLocal bool

	// Local variable
	Offset int64

	// Global variable or function
	IsFunction   bool
	IsDefinition bool
	IsStatic     bool

	// Global variable
	InitData []uint8
	Rel      *Relocation

	// Function
	Params    *Obj
	Body      *AstNode
	Locals    *Obj
	StackSize int64
}

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
type Relocation struct {
	Next   *Relocation
	Offset int64
	Label  string
	Addend int64
}
