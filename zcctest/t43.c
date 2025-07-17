#include <stdio.h>

int main() {
    unsigned int x = 0x00F00000;
    int leading_zeros = __builtin_clz(x);

    printf("Number of leading zeros in %x is %d\n", x, leading_zeros);
    return 0;
}