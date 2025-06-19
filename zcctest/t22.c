#include "../test/test.h"

int main() {
  char buf[100];
  sprintf(buf, "%Lf", (long double)12.3);
  return 0;
}