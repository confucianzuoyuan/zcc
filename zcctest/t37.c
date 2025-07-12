int main() {
  struct SS {
    short x : 7;
  };
  constexpr struct SS ss = {-44};
  return ss.x;
}