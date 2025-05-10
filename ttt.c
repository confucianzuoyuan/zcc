union {
  int a;
  char b[6];
} x;

int main() { return sizeof(x); }