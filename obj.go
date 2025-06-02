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

	// Global variable
	InitData []uint8

	// Function
	Params    *Obj
	Body      *AstNode
	Locals    *Obj
	StackSize int64
}
