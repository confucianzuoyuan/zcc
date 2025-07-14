#include <stdarg.h>

static void va_fn0(int cnt, ...) {
  va_list ap;
  va_start(ap, cnt);
  for (int i = 0; i < cnt; i++)
    va_arg(ap, void (*)(void))();
  va_end(ap);
}

static int garr[2];
static void va_fn1(void) { garr[0] = 111; }
static void va_fn2(void) { garr[1] = 222; }

int main() { va_fn0(2, &va_fn1, &va_fn2); }