package main

import "math"

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
