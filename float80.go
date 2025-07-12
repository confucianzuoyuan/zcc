package main

import (
	"encoding/binary"
	"fmt"
	"math"
	"math/big"
	"strconv"
	"strings"
)

type FloatConst interface {
	isFloat()
	Add(FloatConst) FloatConst
	Sub(FloatConst) FloatConst
	Mul(FloatConst) FloatConst
	Div(FloatConst) FloatConst
	Neg() FloatConst
	Eq(FloatConst) bool
	Ne(FloatConst) bool
	Gt(FloatConst) bool
	Ge(FloatConst) bool
	Lt(FloatConst) bool
	Le(FloatConst) bool
	ToFloat32() float32
	ToFloat64() float64
	ToFloat80() FloatConst80
	ToInt8Slice() []int8
	ToInt64() int64
	ToUInt64() uint64
	IsZero() bool
	IsPositive() bool
	String() string
}

type FloatConst32 struct {
	Value float32
}

func (f FloatConst32) IsPositive() bool {
	return !strings.HasPrefix(fmt.Sprintf("%f", f.Value), "-")
}

func (f FloatConst32) String() string {
	return fmt.Sprintf("f32 %f", f.Value)
}

func (f1 FloatConst32) IsZero() bool {
	return f1.Value == 0
}

func (f1 FloatConst32) ToInt64() int64 {
	return int64(f1.Value)
}

func (f1 FloatConst32) ToUInt64() uint64 {
	return uint64(f1.Value)
}

func (f1 FloatConst32) ToInt8Slice() []int8 {
	return Float32ToInt8Slice(f1.Value)
}

func (f1 FloatConst32) ToFloat32() float32 {
	return f1.Value
}

func (f1 FloatConst32) ToFloat64() float64 {
	return float64(f1.Value)
}

func (f1 FloatConst32) ToFloat80() FloatConst80 {
	f := big.NewFloat(float64(f1.Value))
	return FloatConst80{f}
}

func (f1 FloatConst32) Add(f2 FloatConst) FloatConst {
	switch v := f2.(type) {
	case FloatConst32:
		return FloatConst32{f1.Value + v.Value}
	case FloatConst64:
		return FloatConst64{f1.ToFloat64() + v.Value}
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return FloatConst80{new(big.Float).Add(f.Value, v.Value)}
	default:
		panic("")
	}
}

func (f1 FloatConst32) Sub(f2 FloatConst) FloatConst {
	switch v := f2.(type) {
	case FloatConst32:
		return FloatConst32{f1.Value - v.Value}
	case FloatConst64:
		return FloatConst64{f1.ToFloat64() - v.Value}
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return FloatConst80{new(big.Float).Sub(f.Value, v.Value)}
	default:
		panic("")
	}
}

func (f1 FloatConst32) Mul(f2 FloatConst) FloatConst {
	switch v := f2.(type) {
	case FloatConst32:
		return FloatConst32{f1.Value * v.Value}
	case FloatConst64:
		return FloatConst64{f1.ToFloat64() * v.Value}
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return FloatConst80{new(big.Float).Mul(f.Value, v.Value)}
	default:
		panic("")
	}
}

func (f1 FloatConst32) Div(f2 FloatConst) FloatConst {
	switch v := f2.(type) {
	case FloatConst32:
		return FloatConst32{f1.Value / v.Value}
	case FloatConst64:
		return FloatConst64{f1.ToFloat64() / v.Value}
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return FloatConst80{new(big.Float).Quo(f.Value, v.Value)}
	default:
		panic("")
	}
}

func (f1 FloatConst32) Neg() FloatConst {
	return FloatConst32{-f1.Value}
}

func (f1 FloatConst32) Eq(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value == v.Value
	case FloatConst64:
		return f1.ToFloat64() == v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Eq(v)
	default:
		panic("")
	}
}

func (f1 FloatConst32) Ne(f2 FloatConst) bool {
	// f2_32, ok := f2.(FloatConst32)
	// if !ok {
	// 	panic("Eq: parameter is not FloatConst32")
	// }
	// return f1.Value != f2_32.Value
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value != v.Value
	case FloatConst64:
		return f1.ToFloat64() != v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Ne(v)
	default:
		panic("")
	}
}

func (f1 FloatConst32) Gt(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value > v.Value
	case FloatConst64:
		return f1.ToFloat64() > v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Gt(v)
	default:
		panic("")
	}
}

func (f1 FloatConst32) Ge(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value >= v.Value
	case FloatConst64:
		return f1.ToFloat64() >= v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Ge(v)
	default:
		panic("")
	}
}

func (f1 FloatConst32) Lt(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value < v.Value
	case FloatConst64:
		return f1.ToFloat64() < v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Lt(v)
	default:
		panic("")
	}
}

func (f1 FloatConst32) Le(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value <= v.Value
	case FloatConst64:
		return f1.ToFloat64() <= v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Le(v)
	default:
		panic("")
	}
}

type FloatConst64 struct {
	Value float64
}

func (f1 FloatConst64) IsZero() bool {
	return f1.Value == 0
}

func (f1 FloatConst64) ToInt64() int64 {
	return int64(f1.Value)
}

func (f1 FloatConst64) ToUInt64() uint64 {
	return uint64(f1.Value)
}

func (f1 FloatConst64) ToInt8Slice() []int8 {
	return Float64ToInt8Slice(f1.Value)
}

func (f1 FloatConst64) ToFloat32() float32 {
	return float32(f1.Value)
}

func (f1 FloatConst64) ToFloat64() float64 {
	return f1.Value
}

func (f1 FloatConst64) ToFloat80() FloatConst80 {
	f := big.NewFloat(f1.Value)
	return FloatConst80{f}
}

func (f FloatConst64) String() string {
	return fmt.Sprintf("f64 %f", f.Value)
}

func (f FloatConst64) IsPositive() bool {
	return !strings.HasPrefix(fmt.Sprintf("%f", f.Value), "-")
}

func (f1 FloatConst64) Add(f2 FloatConst) FloatConst {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Add: parameter is not FloatConst32")
	}
	return FloatConst64{f1.Value + f2_64.Value}
}

func (f1 FloatConst64) Sub(f2 FloatConst) FloatConst {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Add: parameter is not FloatConst32")
	}
	return FloatConst64{f1.Value - f2_64.Value}
}

func (f1 FloatConst64) Mul(f2 FloatConst) FloatConst {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Add: parameter is not FloatConst32")
	}
	return FloatConst64{f1.Value * f2_64.Value}
}

func (f1 FloatConst64) Div(f2 FloatConst) FloatConst {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Add: parameter is not FloatConst32")
	}
	return FloatConst64{f1.Value / f2_64.Value}
}

func (f1 FloatConst64) Neg() FloatConst {
	return FloatConst64{-f1.Value}
}

func (f1 FloatConst64) Eq(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value == v.ToFloat64()
	case FloatConst64:
		return f1.ToFloat64() == v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Value.Cmp(v.Value) == 0
	default:
		panic("")
	}
}

func (f1 FloatConst64) Ne(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		return f1.Value != v.ToFloat64()
	case FloatConst64:
		return f1.ToFloat64() != v.Value
	case FloatConst80:
		f := FloatConst80{big.NewFloat(float64(f1.Value))}
		return f.Value.Cmp(v.Value) != 0
	default:
		panic("")
	}
}

func (f1 FloatConst64) Gt(f2 FloatConst) bool {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return f1.Value > f2_64.Value
}

func (f1 FloatConst64) Ge(f2 FloatConst) bool {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return f1.Value >= f2_64.Value
}

func (f1 FloatConst64) Lt(f2 FloatConst) bool {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return f1.Value < f2_64.Value
}

func (f1 FloatConst64) Le(f2 FloatConst) bool {
	f2_64, ok := f2.(FloatConst64)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return f1.Value <= f2_64.Value
}

type FloatConst80 struct {
	Value *big.Float
}

func (f1 FloatConst80) ToInt8Slice() []int8 {
	var arr [10]byte = bigFloatTo80bit(f1.Value)
	result := []int8{}
	for i := 0; i < 10; i++ {
		result = append(result, int8(arr[i]))
	}
	return result
}

func (f1 FloatConst80) ToFloat32() float32 {
	f, _ := f1.Value.Float32()
	return f
}

func (f1 FloatConst80) ToFloat64() float64 {
	f, _ := f1.Value.Float64()
	return f
}

func (f1 FloatConst80) ToFloat80() FloatConst80 {
	return f1
}

func (f1 FloatConst80) ToInt64() int64 {
	f, _ := f1.Value.Int64()
	return int64(f)
}

func (f1 FloatConst80) ToUInt64() uint64 {
	f, _ := f1.Value.Uint64()
	return uint64(f)
}

func (f1 FloatConst80) IsZero() bool {
	zero := big.NewFloat(0)
	return f1.Value.Cmp(zero) == 0
}

func (f FloatConst80) String() string {
	return fmt.Sprintf("f80 %f", f.Value)
}

func (f FloatConst80) IsPositive() bool {
	return !strings.HasPrefix(fmt.Sprintf("%f", f.Value), "-")
}

func (f1 FloatConst80) Add(f2 FloatConst) FloatConst {
	f2_80, ok := f2.(FloatConst80)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return FloatConst80{new(big.Float).Add(f1.Value, f2_80.Value)}
}

func (f1 FloatConst80) Sub(f2 FloatConst) FloatConst {
	f2_80, ok := f2.(FloatConst80)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return FloatConst80{new(big.Float).Sub(f1.Value, f2_80.Value)}
}

func (f1 FloatConst80) Mul(f2 FloatConst) FloatConst {
	f2_80, ok := f2.(FloatConst80)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	return FloatConst80{new(big.Float).Mul(f1.Value, f2_80.Value)}
}

func (f1 FloatConst80) Div(f2 FloatConst) FloatConst {
	f2_80, ok := f2.(FloatConst80)
	if !ok {
		panic("Eq: parameter is not FloatConst32")
	}
	if f2_80.IsZero() && f1.IsZero() {
		f := 0.0
		return FloatConst64{f / f}
	}
	return FloatConst80{new(big.Float).Quo(f1.Value, f2_80.Value)}
}

func (f1 FloatConst80) Neg() FloatConst {
	return FloatConst80{new(big.Float).Neg(f1.Value)}
}

func (f1 FloatConst80) Eq(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Eq(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Eq(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) == 0
	default:
		panic("")
	}
}

func (f1 FloatConst80) Ne(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Ne(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Ne(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) != 0
	default:
		panic("")
	}
}

func (f1 FloatConst80) Gt(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Gt(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Gt(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) == 1
	default:
		panic("")
	}
}

func (f1 FloatConst80) Ge(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Ge(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Ge(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) >= 0
	default:
		panic("")
	}
}

func (f1 FloatConst80) Lt(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Lt(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Lt(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) == -1
	default:
		panic("")
	}
}

func (f1 FloatConst80) Le(f2 FloatConst) bool {
	switch v := f2.(type) {
	case FloatConst32:
		f := FloatConst80{big.NewFloat(float64(v.Value))}
		return f1.Le(f)
	case FloatConst64:
		f := FloatConst80{big.NewFloat(v.Value)}
		return f1.Le(f)
	case FloatConst80:
		return f1.Value.Cmp(v.Value) <= 0
	default:
		panic("")
	}
}

func (FloatConst32) isFloat() {}
func (FloatConst64) isFloat() {}
func (FloatConst80) isFloat() {}

// big.Float 转 80-bit extended precision 字节数组
func bigFloatTo80bit(f *big.Float) [10]byte {
	var zero [10]byte
	if f.Sign() == 0 {
		return zero
	}

	sign := 0
	if f.Sign() < 0 {
		sign = 1
		f = new(big.Float).Neg(f)
	}

	// 设置精度，确保足够高
	prec := uint(128)
	f = new(big.Float).SetPrec(prec).Copy(f)

	// MantExp 拆尾数和指数，尾数范围是[0.5,1)
	mantissaFloat := new(big.Float)
	exp := f.MantExp(mantissaFloat)

	// 调整尾数到[1,2)
	mantissaFloat.Mul(mantissaFloat, big.NewFloat(2))
	exp--

	one := big.NewFloat(1)
	fracPart := new(big.Float).Sub(mantissaFloat, one) // fracPart ∈ [0,1)

	// fracPart * 2^63
	scale := new(big.Float).SetFloat64(float64(uint64(1) << 63))
	fracScaled := new(big.Float).Mul(fracPart, scale)

	fracInt, _ := fracScaled.Uint64()

	mantissa := (uint64(1) << 63) | fracInt

	exponent := uint16(exp + 16383)

	return build80bitFloat(sign, exponent, mantissa)
}

// 构造80-bit extended precision浮点数的10字节表示
func build80bitFloat(sign int, exponent uint16, mantissa uint64) [10]byte {
	var data [10]byte

	// 写入尾数64位（小端）
	binary.LittleEndian.PutUint64(data[0:8], mantissa)

	// 组合符号和指数
	var high uint16 = exponent & 0x7FFF
	if sign != 0 {
		high |= 0x8000
	}

	// 写入指数和符号（小端）
	binary.LittleEndian.PutUint16(data[8:10], high)
	return data
}

// 80-bit extended precision字节数组 转 big.Float
func longDoubleToBigFloat(data [10]byte) *big.Float {
	mantissa := binary.LittleEndian.Uint64(data[0:8])
	high := binary.LittleEndian.Uint16(data[8:10])

	sign := (high >> 15) & 1
	exponent := high & 0x7FFF

	// 处理特殊情况：0 或 非规格化数（这里简化为0）
	if exponent == 0 {
		return big.NewFloat(0)
	}

	exp := int(exponent) - 16383

	// 构造尾数 big.Float
	one := new(big.Float).SetUint64(1 << 63)
	fracPart := new(big.Float).SetUint64(mantissa & ((1 << 63) - 1))

	fMantissa := new(big.Float).Add(one, new(big.Float).Quo(fracPart, new(big.Float).SetFloat64(float64(uint64(1)<<63))))

	// 计算最终值 = mantissa * 2^exp
	// two := big.NewFloat(2)
	expFactor := new(big.Float).SetPrec(256).SetInt(new(big.Int).Exp(big.NewInt(2), big.NewInt(int64(exp)), nil))

	result := new(big.Float).SetPrec(256).Mul(fMantissa, expFactor)

	if sign == 1 {
		result.Neg(result)
	}
	return result
}

// Float80 represents an 80-bit IEEE 754 extended precision floating-point
// value, in x86 extended precision format.
//
// References:
//
//	https://en.wikipedia.org/wiki/Extended_precision#x86_extended_precision_format
type Float80 struct {
	// Sign and exponent.
	//
	//    1 bit:   sign
	//    15 bits: exponent
	se uint16
	// Integer part and fraction.
	//
	//    1 bit:   integer part
	//    63 bits: fraction
	m uint64
}

// Bits returns the IEEE 754 binary representation of f, with the sign and
// exponent in se and the mantissa in m.
func (f Float80) Bits() (se uint16, m uint64) {
	return f.se, f.m
}

// Bytes returns the x86 extended precision binary representation of f as a byte
// slice.
func (f Float80) Bytes() []byte {
	return []byte(f.String())
}

// String returns the IEEE 754 binary representation of f as a string,
// containing 10 bytes in hexadecimal format.
func (f Float80) String() string {
	return fmt.Sprintf("%04X%016X", f.se, f.m)
}

// Float64 returns the float64 representation of f.
func (f Float80) Float64() float64 {
	se := uint64(f.se)
	m := f.m
	// 1 bit: sign
	sign := se >> 15
	// 15 bits: exponent
	exp := se & 0x7FFF
	// Adjust for exponent bias.
	//
	// === [ binary64 ] =========================================================
	//
	// Exponent bias 1023.
	//
	//    +===========================+=======================+
	//    | Exponent (in binary)      | Notes                 |
	//    +===========================+=======================+
	//    | 00000000000               | zero/subnormal number |
	//    +---------------------------+-----------------------+
	//    | 00000000001 - 11111111110 | normalized value      |
	//    +---------------------------+-----------------------+
	//    | 11111111111               | infinity/NaN          |
	//    +---------------------------+-----------------------+
	//
	// References:
	//    https://en.wikipedia.org/wiki/Double-precision_floating-point_format#Exponent_encoding
	exp64 := int64(exp) - 16383 + 1023
	switch {
	case exp == 0:
		// exponent is all zeroes.
		exp64 = 0
	case exp == 0x7FFF:
		switch m {
		case 0xC000000000000000:
			// Handle NaN.
			return math.NaN()
		}
		// exponent is all ones.
		exp64 = 0x7FF
	default:
	}
	// 63 bits: fraction
	frac := m & 0x7FFFFFFFFFFFFFFF
	// Sign, exponent and fraction of binary64.
	//
	//    1 bit:   sign
	//    11 bits: exponent
	//    52 bits: fraction
	//
	// References:
	//    https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
	bits := sign<<63 | uint64(exp64)<<52 | frac>>11
	return math.Float64frombits(bits)
}

// BigFloat returns the *big.Float representation of f.
func (f Float80) BigFloat() *big.Float {
	panic("not yet implemented")
}

// NewFromBits returns a new 80-bit floating-point value based on the sign,
// exponent and mantissa bits.
func NewFromBits(se uint16, m uint64) Float80 {
	return Float80{
		se: se,
		m:  m,
	}
}

// NewFromBytes returns a new 80-bit floating-point value based on b, which
// contains 20 bytes in hexadecimal format.
func NewFromBytes(b []byte) Float80 {
	return NewFromString(string(b))
}

// NewFromString returns a new 80-bit floating-point value based on s, which
// contains 20 bytes in hexadecimal format.
func NewFromString(s string) Float80 {
	if len(s) != 20 {
		panic(fmt.Errorf("invalid length of float80 hexadecimal representation, expected 20, got %d", len(s)))
	}
	se, err := strconv.ParseUint(s[:4], 16, 16)
	if err != nil {
		panic(err)
	}
	m, err := strconv.ParseUint(s[4:], 16, 64)
	if err != nil {
		panic(err)
	}
	return NewFromBits(uint16(se), m)
}

// NewFromFloat64 returns the nearest 80-bit floating-point value for x.
func NewFromFloat64(x float64) Float80 {
	// Sign, exponent and fraction of binary64.
	//
	//    1 bit:   sign
	//    11 bits: exponent
	//    52 bits: fraction
	bits := math.Float64bits(x)
	// 1 bit: sign
	sign := uint16(bits >> 63)
	// 11 bits: exponent
	exp := bits >> 52 & 0x7FF
	// 52 bits: fraction
	frac := bits & 0xFFFFFFFFFFFFF

	if exp == 0 && frac == 0 {
		// zero value.
		se := sign << 15
		return Float80{se: se}
	}

	// Sign, exponent and fraction of binary80.
	//
	//    1 bit:   sign
	//    15 bits: exponent
	//    1 bit:   integer part
	//    63 bits: fraction

	// 15 bits: exponent.
	//
	// Exponent bias 1023  (binary64)
	// Exponent bias 16383 (binary80)
	exp80 := int64(exp) - 1023 + 16383
	// 63 bits: fraction.
	//
	frac80 := frac << 11
	switch {
	case exp == 0:
		exp80 = 0
	case exp == 0x7FF:
		exp80 = 0x7FFF
	}
	se := sign<<15 | uint16(exp80)
	// Integer part set to specify normalized value.

	// Handle NaN.
	if math.IsNaN(x) {
		return NewFromBits(se, 0xC000000000000000)
	}

	m := 0x8000000000000000 | frac80
	return NewFromBits(se, m)
}
