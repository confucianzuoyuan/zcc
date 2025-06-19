int main() {
  int i = 0;
  switch (7) {
  case 0 ... 5:
    i = 1;
    break;
  case 6 ... 20:
    i = 2;
    break;
  }
  return i;
}