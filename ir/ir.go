package ir

import (
	"fmt"
	"strings"
)

type IrCmpKind uint8

const (
	// Returns 1 if first value is less than second, respecting signedness
	Slt IrCmpKind = iota
	// Returns 1 if first value is less than or equal to second, respecting signedness
	Sle
	// Returns 1 if first value is greater than second, respecting signedness
	Sgt
	// Returns 1 if first value is greater than or equal to second, respecting signedness
	Sge
	// Returns 1 if values are equal
	Eq
	// Returns 1 if values are not equal
	Ne
	// Returns 1 if both operands are not NaN (ordered comparison)
	O
	// Returns 1 if at least one operand is NaN (unordered comparison)
	Uo
	// Returns 1 if first value is less than second, unsigned comparison
	Ult
	// Returns 1 if first value is less than or equal to second, unsigned comparison
	Ule
	// Returns 1 if first value is greater than second, unsigned comparison
	Ugt
	// Returns 1 if first value is greater than or equal to second, unsigned comparison
	Uge
)

type IrValue interface {
	isIrValue()
	String() string
}

type IrValueTemporary struct {
	// Temporary name
	Name string
}

type IrValueGlobal struct {
	// Global name
	Name string
}

type IrValueConst struct {
	// Constant value
	Value uint64
}

func (*IrValueTemporary) isIrValue() {}
func (*IrValueGlobal) isIrValue()    {}
func (*IrValueConst) isIrValue()     {}

func (v *IrValueTemporary) String() string {
	return "%" + v.Name
}

func (v *IrValueGlobal) String() string {
	return "$" + v.Name
}

func (v *IrValueConst) String() string {
	return fmt.Sprintf("%d", v.Value)
}

type IrType interface {
	isIrType()
	String() string
	IntoABI() IrType
	IntoBase() IrType
	Size() uint64
	Align() uint64
}

// Base types
type IrTypeWord struct{}
type IrTypeLong struct{}
type IrTypeSingle struct{}
type IrTypeDouble struct{}

// Internal types
type IrTypeZero struct{}

// Extended types
type IrTypeByte struct{}
type IrTypeSignedByte struct{}
type IrTypeUnsignedByte struct{}
type IrTypeHalfword struct{}
type IrTypeSignedHalfword struct{}
type IrTypeUnsignedHalfword struct{}

// Aggregate type with a specified name
type IrTypeAggregate struct {
	Type IrType
}

// 以下函数不会被调用，只是弥补golang的类型系统没有tagged union的缺陷
func (*IrTypeWord) isIrType()             {}
func (*IrTypeLong) isIrType()             {}
func (*IrTypeSingle) isIrType()           {}
func (*IrTypeDouble) isIrType()           {}
func (*IrTypeZero) isIrType()             {}
func (*IrTypeByte) isIrType()             {}
func (*IrTypeSignedByte) isIrType()       {}
func (*IrTypeUnsignedByte) isIrType()     {}
func (*IrTypeHalfword) isIrType()         {}
func (*IrTypeSignedHalfword) isIrType()   {}
func (*IrTypeUnsignedHalfword) isIrType() {}
func (*IrTypeAggregate) isIrType()        {}

func (*IrTypeByte) String() string             { return "b" }
func (*IrTypeSignedByte) String() string       { return "sb" }
func (*IrTypeUnsignedByte) String() string     { return "ub" }
func (*IrTypeHalfword) String() string         { return "h" }
func (*IrTypeSignedHalfword) String() string   { return "sh" }
func (*IrTypeUnsignedHalfword) String() string { return "uh" }
func (*IrTypeWord) String() string             { return "w" }
func (*IrTypeLong) String() string             { return "l" }
func (*IrTypeSingle) String() string           { return "s" }
func (*IrTypeDouble) String() string           { return "d" }
func (*IrTypeZero) String() string             { return "z" }
func (t *IrTypeAggregate) String() string {
	if t.Type != nil {
		return fmt.Sprintf("a(%s)", t.Type.String())
	}
	return "a()"
}

// Returns a C ABI type. Extended types are converted to closest base types
func (*IrTypeByte) IntoABI() IrType             { return &IrTypeWord{} }
func (*IrTypeSignedByte) IntoABI() IrType       { return &IrTypeWord{} }
func (*IrTypeUnsignedByte) IntoABI() IrType     { return &IrTypeWord{} }
func (*IrTypeHalfword) IntoABI() IrType         { return &IrTypeWord{} }
func (*IrTypeSignedHalfword) IntoABI() IrType   { return &IrTypeWord{} }
func (*IrTypeUnsignedHalfword) IntoABI() IrType { return &IrTypeWord{} }
func (*IrTypeWord) IntoABI() IrType             { return &IrTypeWord{} }
func (*IrTypeLong) IntoABI() IrType             { return &IrTypeLong{} }
func (*IrTypeSingle) IntoABI() IrType           { return &IrTypeSingle{} }
func (*IrTypeDouble) IntoABI() IrType           { return &IrTypeDouble{} }
func (*IrTypeZero) IntoABI() IrType             { return &IrTypeWord{} }
func (t *IrTypeAggregate) IntoABI() IrType      { return t }

// Returns the closest base type
func (*IrTypeByte) IntoBase() IrType             { return &IrTypeWord{} }
func (*IrTypeSignedByte) IntoBase() IrType       { return &IrTypeWord{} }
func (*IrTypeUnsignedByte) IntoBase() IrType     { return &IrTypeWord{} }
func (*IrTypeHalfword) IntoBase() IrType         { return &IrTypeWord{} }
func (*IrTypeSignedHalfword) IntoBase() IrType   { return &IrTypeWord{} }
func (*IrTypeUnsignedHalfword) IntoBase() IrType { return &IrTypeWord{} }
func (*IrTypeWord) IntoBase() IrType             { return &IrTypeWord{} }
func (*IrTypeLong) IntoBase() IrType             { return &IrTypeLong{} }
func (*IrTypeSingle) IntoBase() IrType           { return &IrTypeSingle{} }
func (*IrTypeDouble) IntoBase() IrType           { return &IrTypeDouble{} }
func (*IrTypeZero) IntoBase() IrType             { return &IrTypeZero{} }
func (t *IrTypeAggregate) IntoBase() IrType      { return &IrTypeLong{} }

// Returns byte size for values of the type
func (*IrTypeByte) Size() uint64             { return 1 }
func (*IrTypeSignedByte) Size() uint64       { return 1 }
func (*IrTypeUnsignedByte) Size() uint64     { return 1 }
func (*IrTypeZero) Size() uint64             { return 1 }
func (*IrTypeHalfword) Size() uint64         { return 2 }
func (*IrTypeSignedHalfword) Size() uint64   { return 2 }
func (*IrTypeUnsignedHalfword) Size() uint64 { return 2 }
func (*IrTypeWord) Size() uint64             { return 4 }
func (*IrTypeSingle) Size() uint64           { return 4 }
func (*IrTypeLong) Size() uint64             { return 8 }
func (*IrTypeDouble) Size() uint64           { return 8 }
func (t *IrTypeAggregate) Size() uint64      { return 1 } // todo

// Returns byte alignment for values of the type
func (*IrTypeByte) Align() uint64             { return 1 }
func (*IrTypeSignedByte) Align() uint64       { return 1 }
func (*IrTypeUnsignedByte) Align() uint64     { return 1 }
func (*IrTypeZero) Align() uint64             { return 1 }
func (*IrTypeHalfword) Align() uint64         { return 2 }
func (*IrTypeSignedHalfword) Align() uint64   { return 2 }
func (*IrTypeUnsignedHalfword) Align() uint64 { return 2 }
func (*IrTypeWord) Align() uint64             { return 4 }
func (*IrTypeSingle) Align() uint64           { return 4 }
func (*IrTypeLong) Align() uint64             { return 8 }
func (*IrTypeDouble) Align() uint64           { return 8 }
func (t *IrTypeAggregate) Align() uint64      { return 1 }

type IrDataItemWithType struct {
	Type IrType
	Item IrDataItem
}

func (d *IrDataItemWithType) String() string {
	return fmt.Sprintf("%s %s", d.Type.String(), d.Item.String())
}

type IrDataDef struct {
	Linkage *IrLinkage
	Name    string
	Align   int64
	Items   []IrDataItemWithType
}

func (d *IrDataDef) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("%sdata $%s = ", d.Linkage.String(), d.Name))

	if d.Align != -1 {
		sb.WriteString(fmt.Sprintf("align %d ", d.Align))
	}

	sb.WriteString("{ ")

	for idx, item := range d.Items {
		if idx != 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(item.String())
	}

	sb.WriteString(" }")

	return sb.String()
}

type IrDataItem interface {
	isIrDataItem()
	String() string
}

// Symbol and offset
type IrDataItemSymbol struct {
	Name   string
	Offset int64
}

type IrDataItemStr struct {
	Str string
}

type IrDataItemConst struct {
	Value uint64
}

// Zero-initialized data of specified size
type IrDataItemZero struct {
	Size uint64
}

func (*IrDataItemSymbol) isIrDataItem() {}
func (*IrDataItemStr) isIrDataItem()    {}
func (*IrDataItemConst) isIrDataItem()  {}
func (*IrDataItemZero) isIrDataItem()   {}

func (d *IrDataItemSymbol) String() string {
	if d.Offset == -1 {
		return "$" + d.Name
	} else {
		return "$" + d.Name + " +" + fmt.Sprintf("%d", d.Offset)
	}
}
func (d *IrDataItemStr) String() string   { return "\"" + d.Str + "\"" }
func (d *IrDataItemConst) String() string { return fmt.Sprintf("%d", d.Value) }
func (d *IrDataItemZero) String() string  { return fmt.Sprintf("z %d", d.Size) }

type IrTypeDefItem struct {
	Type  IrType
	Count uint64
}

type IrTypeDef struct {
	Name  string
	Align int64
	Items []IrTypeDefItem
}

type Instr interface {
	isInstr()
	String() string
}

// Adds values of two temporaries together
type AddInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Subtracts the second value from the first one
type SubInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Multiplies values of two temporaries
type MulInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Divides the first value by the second one
type DivInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Returns a remainder from division
type RemInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Performs a comparion between values
type CmpInstr struct {
	Lhs  IrValue
	Rhs  IrValue
	Kind IrCmpKind
	Type IrType
}

// Performs a bitwise AND on values
type AndInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Performs a bitwise OR on values
type OrInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Copies either a temporary or a literal value
type CopyInstr struct {
	Value IrValue
}

// Return from a function, optionally with a value
type RetInstr struct {
	Value IrValue
}

// Jumps to first label if a value is nonzero or to the second one otherwise
type JnzInstr struct {
	Cond           IrValue
	IfNonZeroLabel string
	IfZeroLabel    string
}

// Unconditionally jumps to a label
type JmpInstr struct {
	Label string
}

// Calls a function
type CallInstr struct {
	// Function name
	Name string
	// Function arguments
	Args []IrFunctionArg
	// Return value
	OptVariadicI int64
}

// Allocates a 4-byte aligned area on the stack
type Alloc4Instr struct {
	// Size of the area
	Size uint32
}

// Allocates a 8-byte aligned area on the stack
type Alloc8Instr struct {
	// Size of the area
	Size uint64
}

// Allocates a 16-byte aligned area on the stack
type Alloc16Instr struct {
	// Size of the area
	Size uint64
}

// Stores a value into memory pointed to by destination.
// `(type, destination, value)`
type StoreInstr struct {
	Type  IrType
	Dst   IrValue
	Value IrValue
}

// Loads a value from memory pointed to by source
// `(type, source)`
type LoadInstr struct {
	Type IrType
	Src  IrValue
}

// `(source, destination, n)`
//
// Copy `n` bytes from the source address to the destination address.
//
// n must be a constant value.
//
// ## Minimum supported QBE version
// `1.1`
type BlitInstr struct {
	// Source address
	Src IrValue
	// Destination address
	Dst IrValue
	// Number of bytes to copy
	// Must be a constant value
	Size uint64
}

// Debug file.
type DbgFile struct {
	// File name
	Name string
}

// Debug line.
//
// Takes line number and an optional column.
type DbgLoc struct {
	// Line number
	Line int64
	// Column number
	Col int64
}

// Unsigned arithmetic

// Performs unsigned division of the first value by the second one
type UdivInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Returns the remainder from unsigned division
type UremInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Shifts

// Shift arithmetic right (preserves sign)
type SarInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Shift logical right (fills with zeros)
type ShrInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Shift left (fills with zeros)
type ShlInstr struct {
	Lhs IrValue
	Rhs IrValue
}

// Type conversions

// Cast between integer and floating point of the same width
type CastInstr struct {
	Value IrValue
}

// Extension operations

// Sign-extends a word to a long
type ExtswInstr struct {
	Value IrValue
}

// Zero-extends a word to a long
type ExtuwInstr struct {
	Value IrValue
}

// Sign-extends a halfword to a word or long
type ExtshInstr struct {
	Value IrValue
}

// Zero-extends a halfword to a word or long
type ExtuhInstr struct {
	Value IrValue
}

// Sign-extends a byte to a word or long
type ExtsbInstr struct {
	Value IrValue
}

// Zero-extends a byte to a word or long
type ExtubInstr struct {
	Value IrValue
}

// Extends a single-precision float to double-precision
type ExtsInstr struct {
	Value IrValue
}

// Truncates a double-precision float to single-precision
type TruncdInstr struct {
	Value IrValue
}

// Float-integer conversions

// Converts a single-precision float to a signed integer
type StosiInstr struct {
	Value IrValue
}

// Converts a single-precision float to an unsigned integer
type StouiInstr struct {
	Value IrValue
}

// Converts a double-precision float to a signed integer
type DtosiInstr struct {
	Value IrValue
}

// Converts a double-precision float to an unsigned integer
type DtouiInstr struct {
	Value IrValue
}

// Converts a signed word to a float
type SwtofInstr struct {
	Value IrValue
}

// Converts an unsigned word to a float
type UwtofInstr struct {
	Value IrValue
}

// Converts a signed long to a float
type SltofInstr struct {
	Value IrValue
}

// Converts an unsigned long to a float
type UltofInstr struct {
	Value IrValue
}

// Variadic function support

// Initializes a variable argument list
type VaStartInstr struct {
	// Address of the variable argument list
	Value IrValue
}

// Fetches the next argument from a variable argument list
type VaArgInstr struct {
	// Address of the variable argument list
	Value IrValue
	// Type of the argument
	Type IrType
}

// Phi instruction

// Selects value based on the control flow path into a block.
type PhiInstr struct {
	S1 string
	V1 IrValue
	S2 string
	V2 IrValue
}

// Program termination

// Terminates the program with an error
type HltInstr struct{}

func (*AddInstr) isInstr()     {}
func (*SubInstr) isInstr()     {}
func (*MulInstr) isInstr()     {}
func (*DivInstr) isInstr()     {}
func (*RemInstr) isInstr()     {}
func (*CmpInstr) isInstr()     {}
func (*AndInstr) isInstr()     {}
func (*OrInstr) isInstr()      {}
func (*CopyInstr) isInstr()    {}
func (*RetInstr) isInstr()     {}
func (*JnzInstr) isInstr()     {}
func (*JmpInstr) isInstr()     {}
func (*CallInstr) isInstr()    {}
func (*Alloc4Instr) isInstr()  {}
func (*Alloc8Instr) isInstr()  {}
func (*Alloc16Instr) isInstr() {}
func (*StoreInstr) isInstr()   {}
func (*LoadInstr) isInstr()    {}
func (*BlitInstr) isInstr()    {}
func (*DbgFile) isInstr()      {}
func (*DbgLoc) isInstr()       {}
func (*UdivInstr) isInstr()    {}
func (*UremInstr) isInstr()    {}
func (*SarInstr) isInstr()     {}
func (*ShrInstr) isInstr()     {}
func (*ShlInstr) isInstr()     {}
func (*CastInstr) isInstr()    {}
func (*ExtswInstr) isInstr()   {}
func (*ExtuwInstr) isInstr()   {}
func (*ExtshInstr) isInstr()   {}
func (*ExtuhInstr) isInstr()   {}
func (*ExtsbInstr) isInstr()   {}
func (*ExtubInstr) isInstr()   {}
func (*ExtsInstr) isInstr()    {}
func (*TruncdInstr) isInstr()  {}
func (*StosiInstr) isInstr()   {}
func (*StouiInstr) isInstr()   {}
func (*DtosiInstr) isInstr()   {}
func (*DtouiInstr) isInstr()   {}
func (*SwtofInstr) isInstr()   {}
func (*UwtofInstr) isInstr()   {}
func (*SltofInstr) isInstr()   {}
func (*VaStartInstr) isInstr() {}
func (*VaArgInstr) isInstr()   {}
func (*PhiInstr) isInstr()     {}
func (*HltInstr) isInstr()     {}

func (i *AddInstr) String() string     { return fmt.Sprintf("add %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *SubInstr) String() string     { return fmt.Sprintf("sub %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *MulInstr) String() string     { return fmt.Sprintf("mul %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *DivInstr) String() string     { return fmt.Sprintf("div %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *RemInstr) String() string     { return fmt.Sprintf("rem %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *AndInstr) String() string     { return fmt.Sprintf("and %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *OrInstr) String() string      { return fmt.Sprintf("or %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *CopyInstr) String() string    { return fmt.Sprintf("copy %s", i.Value.String()) }
func (i *JmpInstr) String() string     { return fmt.Sprintf("jmp @%s", i.Label) }
func (i *Alloc4Instr) String() string  { return fmt.Sprintf("alloc4 %d", i.Size) }
func (i *Alloc8Instr) String() string  { return fmt.Sprintf("alloc8 %d", i.Size) }
func (i *Alloc16Instr) String() string { return fmt.Sprintf("alloc16 %d", i.Size) }
func (i *BlitInstr) String() string {
	return fmt.Sprintf("blit %s, %s, %d", i.Src.String(), i.Dst.String(), i.Size)
}
func (i *UdivInstr) String() string {
	return fmt.Sprintf("udiv %s, %s", i.Lhs.String(), i.Rhs.String())
}
func (i *UremInstr) String() string {
	return fmt.Sprintf("urem %s, %s", i.Lhs.String(), i.Rhs.String())
}
func (i *SarInstr) String() string     { return fmt.Sprintf("sar %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *ShrInstr) String() string     { return fmt.Sprintf("shr %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *ShlInstr) String() string     { return fmt.Sprintf("shl %s, %s", i.Lhs.String(), i.Rhs.String()) }
func (i *CastInstr) String() string    { return fmt.Sprintf("cast %s", i.Value.String()) }
func (i *ExtswInstr) String() string   { return fmt.Sprintf("extsw %s", i.Value.String()) }
func (i *ExtuwInstr) String() string   { return fmt.Sprintf("extuw %s", i.Value.String()) }
func (i *ExtshInstr) String() string   { return fmt.Sprintf("extsh %s", i.Value.String()) }
func (i *ExtuhInstr) String() string   { return fmt.Sprintf("extuh %s", i.Value.String()) }
func (i *ExtsbInstr) String() string   { return fmt.Sprintf("extsb %s", i.Value.String()) }
func (i *ExtubInstr) String() string   { return fmt.Sprintf("extub %s", i.Value.String()) }
func (i *ExtsInstr) String() string    { return fmt.Sprintf("exts %s", i.Value.String()) }
func (i *TruncdInstr) String() string  { return fmt.Sprintf("truncd %s", i.Value.String()) }
func (i *StosiInstr) String() string   { return fmt.Sprintf("stosi %s", i.Value.String()) }
func (i *StouiInstr) String() string   { return fmt.Sprintf("stoui %s", i.Value.String()) }
func (i *DtosiInstr) String() string   { return fmt.Sprintf("dtosi %s", i.Value.String()) }
func (i *DtouiInstr) String() string   { return fmt.Sprintf("dtoui %s", i.Value.String()) }
func (i *SwtofInstr) String() string   { return fmt.Sprintf("swtof %s", i.Value.String()) }
func (i *UwtofInstr) String() string   { return fmt.Sprintf("uwtof %s", i.Value.String()) }
func (i *SltofInstr) String() string   { return fmt.Sprintf("sltof %s", i.Value.String()) }
func (i *UltofInstr) String() string   { return fmt.Sprintf("ultof %s", i.Value.String()) }
func (i *VaStartInstr) String() string { return fmt.Sprintf("vastart %s", i.Value.String()) }
func (i *VaArgInstr) String() string {
	return fmt.Sprintf("vaarg %s, %s", i.Value.String(), i.Type.String())
}
func (i *PhiInstr) String() string {
	return fmt.Sprintf("phi @%s %s, @%s %s", i.S1, i.V1.String(), i.S2, i.V2.String())
}
func (i *HltInstr) String() string { return "hlt" }
func (i *CmpInstr) String() string {
	switch i.Type.(type) {
	case *IrTypeAggregate:
		panic("cannot compare aggregate types")
	default:
	}

	switch i.Kind {
	case Slt:
		return fmt.Sprintf("cslt%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Sle:
		return fmt.Sprintf("csle%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Sgt:
		return fmt.Sprintf("csgt%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Sge:
		return fmt.Sprintf("csge%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Eq:
		return fmt.Sprintf("ceq%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Ne:
		return fmt.Sprintf("cne%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case O:
		return fmt.Sprintf("co%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Uo:
		return fmt.Sprintf("cuo%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Ult:
		return fmt.Sprintf("cult%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Ule:
		return fmt.Sprintf("cule%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Ugt:
		return fmt.Sprintf("cugt%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	case Uge:
		return fmt.Sprintf("cuge%s %s, %s", i.Type.String(), i.Lhs.String(), i.Rhs.String())
	}

	panic("unknown comparison kind")
}
func (i *DbgFile) String() string { return fmt.Sprintf("dbgfile \"%s\"", i.Name) }
func (i *DbgLoc) String() string {
	if i.Col == -1 {
		return fmt.Sprintf("dbgloc %d", i.Line)
	} else {
		return fmt.Sprintf("dbgloc %d, %d", i.Line, i.Col)
	}
}
func (i *JnzInstr) String() string {
	return fmt.Sprintf("jnz %s, @%s, @%s", i.Cond.String(), i.IfNonZeroLabel, i.IfZeroLabel)
}
func (i *RetInstr) String() string {
	if i.Value != nil {
		return fmt.Sprintf("ret %s", i.Value.String())
	}
	return "ret"
}
func (i *StoreInstr) String() string {
	switch i.Type.(type) {
	case *IrTypeAggregate:
		panic("unimplement store to an aggregate type")
	default:
	}
	return fmt.Sprintf("store%s, %s, %s", i.Type.String(), i.Dst.String(), i.Value.String())
}
func (i *LoadInstr) String() string {
	switch i.Type.(type) {
	case *IrTypeAggregate:
		panic("unimplement load from an aggregate type")
	default:
	}
	return fmt.Sprintf("load%s, %s", i.Type.String(), i.Src.String())
}

func insert(slice []string, index int64, value string) []string {
	// 创建一个新的切片，大小为原切片的大小 + 1
	newSlice := make([]string, len(slice)+1)

	// 复制插入位置之前的元素
	copy(newSlice, slice[:index])

	// 插入新元素
	newSlice[index] = value

	// 复制插入位置之后的元素
	copy(newSlice[index+1:], slice[index:])

	return newSlice
}
func (i *CallInstr) String() string {
	argsFmt := []string{}
	for _, arg := range i.Args {
		argsFmt = append(argsFmt, arg.Type.String()+" "+arg.Value.String())
	}

	if i.OptVariadicI != -1 {
		argsFmt = insert(argsFmt, i.OptVariadicI, "...")
	}

	return "call $" + i.Name + "(" + strings.Join(argsFmt, ", ") + ")"
}

// An IR statement
type IrStmt interface {
	isIrStmt()
	String() string
}

type AssignStmt struct {
	Temp  IrValue
	Type  IrType
	Instr Instr
}

type VolatileStmt struct {
	Instr Instr
}

func (*AssignStmt) isIrStmt()   {}
func (*VolatileStmt) isIrStmt() {}

func (s *AssignStmt) String() string {
	switch s.Temp.(type) {
	case *IrValueTemporary:
		return fmt.Sprintf("%s =%s %s", s.Temp.String(), s.Type.String(), s.Instr.String())
	default:
		panic("cannot assign to non-temporary value")
	}
}
func (s *VolatileStmt) String() string { return s.Instr.String() }

type IrBlockItem interface {
	isIrBlockItem()
	String() string
}

type IrStatementBlockItem struct {
	S IrStmt
}

type IrCommentBlockItem struct {
	C string
}

func (*IrStatementBlockItem) isIrBlockItem() {}
func (*IrCommentBlockItem) isIrBlockItem()   {}

func (s *IrStatementBlockItem) String() string { return s.S.String() }
func (s *IrCommentBlockItem) String() string   { return "# " + s.C }

type IrBlock struct {
	// Label before the block
	Label string
	// A list of statements in the block
	Items []IrBlockItem
}

func (b *IrBlock) String() string {
	var sb strings.Builder
	sb.WriteString("@" + b.Label + "\n")
	for _, item := range b.Items {
		sb.WriteString("\t" + item.String() + "\n")
	}
	return sb.String()
}

// Linkage of a function or data defintion (e.g. section and
// private/public status)
type IrLinkage struct {
	// Specifies whether the target is going to be accessible publicly
	Exported bool
	// Specifies target's section
	Section string
	// Specifies target's section flags
	SecFlags string
	// Specifies whether the target is stored in thread-local storage
	ThreadLocal bool
}

func (l *IrLinkage) String() string {
	res := ""
	if l.Exported {
		res += "exported "
	}
	if l.ThreadLocal {
		res += "thread "
	}
	if l.Section != "" {
		res += "section \"" + l.Section + "\""
		if l.SecFlags != "" {
			res += " \"" + l.SecFlags + "\""
		}
		res += " "
	}
	return res
}

func publicLinkage() *IrLinkage {
	return &IrLinkage{
		Exported:    true,
		Section:     "",
		SecFlags:    "",
		ThreadLocal: false,
	}
}

type IrFunctionArg struct {
	Type  IrType
	Value IrValue
}

type IrFunction struct {
	// Function's linkage
	Linkage *IrLinkage
	// Function name
	Name string
	// Function arguments
	Arguments []IrFunctionArg
	// Return type
	RetType IrType
	// Labelled blocks
	Blocks []IrBlock
}

func (f *IrFunction) String() string {
	var sb strings.Builder
	sb.WriteString(f.Linkage.String())
	sb.WriteString("function")
	if f.RetType != nil {
		sb.WriteString(" " + f.RetType.String())
	}

	sb.WriteString("$" + f.Name + "(")
	for i, arg := range f.Arguments {
		if i != 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(arg.Type.String() + " " + arg.Value.String())
	}
	sb.WriteString(") {\n")

	for idx, block := range f.Blocks {
		sb.WriteString(block.String())
		if idx != len(f.Blocks)-1 {
			sb.WriteString("\n")
		}
	}
	sb.WriteString("}\n")

	return sb.String()
}

type IrModule struct {
	Functions []IrFunction
	Types     []IrTypeDef
	Data      []IrDataDef
}
