struct {
  char a;
  int b : 5;
  int c : 10;
} g45 = {1, 2, 3};

int main() { return g45.b; }