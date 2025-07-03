package main

import (
	"encoding/binary"
	"math"
)

func Float64ToInt8Slice(f float64) []int8 {
	// 将 float64 转换为 uint64 的二进制表示
	bits := math.Float64bits(f)

	// 创建一个长度为8的 int8 切片
	var b [8]int8

	// 逐字节提取 uint64 的每个字节
	for i := 0; i < 8; i++ {
		// 取最低8位，并右移
		b[i] = int8(bits >> (8 * i) & 0xff)
	}
	return b[:]
}

func Float32ToInt8Slice(f float32) []int8 {
	// 将 float64 转换为 uint64 的二进制表示
	bits := math.Float32bits(f)

	// 创建一个长度为8的 int8 切片
	var b [4]int8

	// 逐字节提取 uint64 的每个字节
	for i := 0; i < 4; i++ {
		// 取最低8位，并右移
		b[i] = int8(bits >> (8 * i) & 0xff)
	}
	return b[:]
}

func Int8SliceToFloat32(data []int8) float32 {
	b := make([]byte, len(data))
	for i, v := range data {
		b[i] = byte(v)
	}
	bits := binary.LittleEndian.Uint32(b)
	return math.Float32frombits(bits)
}

func Int8SliceToFloat64(data []int8) float64 {
	b := make([]byte, len(data))
	for i, v := range data {
		b[i] = byte(v)
	}
	bits := binary.LittleEndian.Uint64(b)
	return math.Float64frombits(bits)
}

func Int64ToInt8Slice(n int64) []int8 {
	result := make([]int8, 8)
	for i := 0; i < 8; i++ {
		// 右移 i*8 位，然后取最低 8 位，转换成 int8
		result[i] = int8((n >> (8 * i)) & 0xFF)
	}
	return result
}
