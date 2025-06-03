int main() {
  int i = 0;
  goto a;
a:
  i++;
b:
  i++;
c:
  i++;
  return i;
}