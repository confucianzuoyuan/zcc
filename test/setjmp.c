#include "test.h"
#include <setjmp.h>

int acc;
jmp_buf jbuf;

void fn(int count)
{
    acc += count;
    longjmp(jbuf, count+1);
}

int main(void)
{
    volatile int count = 0;

    if (setjmp(jbuf) < 5)
      fn(++count);

    ASSERT(10, acc);
    printf("OK\n");
}

