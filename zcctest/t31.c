static constexpr float f = 11.0;
constexpr long double ld = 22.0;

int main() {
  _Static_assert(ld == f * 2);
  return 0;
}