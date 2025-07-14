#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#define ASSERT(x, y) test_assert((int)(x), (int)(y), #y)
#define DASSERT(x) static_assert(x,""); ASSERT(1, x)
#define EASSERT(x,y) do{static_assert((int)(x) == (int)(y),"");}while(0); ASSERT(x, y)

#if defined(__zcc__) || defined(__clang__)
#define NOTGCC
#endif
#if defined(__zcc__) || !defined(__clang__)
#define NOTCLANG
#endif

extern
#if defined __cplusplus
"C"
#endif
void test_assert(int expected, int actual, char *code);
