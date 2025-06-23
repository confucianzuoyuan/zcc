#include "string.h"
#define EMPTY()
#define H(x) #x
#define STR(x) H(x)

int main() {
  return strcmp("+ ()", strcmp("+ ()", STR(+EMPTY()
()) ));
}