#include <stdio.h>

typedef unsigned long long ptr;

ptr scheme_entry();

int main() {
  printf("%lld\n", scheme_entry());
  return 0;
}
