constexpr int i = 7;
constexpr int arr[i] = { 1,2,3,4,5,6,7 };

int main(void){
  constexpr int j = arr[(i - 1)];
  return 0;
}