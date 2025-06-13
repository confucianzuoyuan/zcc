typedef struct {
  int a, b;
  short c;
  char d;
} Ty4;

Ty4 struct_test24(void) { return (Ty4){10, 20, 30, 40}; }

int main() { return struct_test24().a; }