int main() {
    return _Generic(100.0, double: 1, int *: 2, int: 3, float: 4);
}