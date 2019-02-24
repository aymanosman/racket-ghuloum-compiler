#include <stdio.h>
#include <string.h>

/* define all scheme constants */
#define bool_f 0x2F
#define bool_t 0x6F
#define nil    0x3F

#define fixnum_mask 0x03
#define fixnum_tag 0x00
#define fixnum_shift 2

#define char_mask 0xFF
#define char_tag 0x0F
#define char_shift 8

/* all scheme values are of type ptrs */
typedef unsigned long long ptr;

ptr scheme_entry();

void print_char(char c) {
  switch (c) {
  case '\t':
    printf("#\\%s", "tab");
    break;
  case '\n':
    printf("#\\%s", "newline");
    break;
  case '\r':
    printf("#\\%s", "return");
    break;
  case ' ':
    printf("#\\%s", "space");
    break;
  default: {
    printf("#\\%c", c);
  }
  }
}

static void print_ptr(ptr x){
  if((x & fixnum_mask) == fixnum_tag){
    printf("%d", ((int)x) >> fixnum_shift);
  } else if (x == bool_f){
    printf("#f");
  } else if (x == bool_t){
    printf("#t");
  } else if (x == nil){
    printf("()");
  } else if ((x & char_mask) == char_tag){
    print_char((char)((int)x >> char_shift));
  } else {
    printf("#<unknown 0x%p>", (void*)x);
  }
  printf("\n");
}

int main() {
  print_ptr(scheme_entry());
  return 0;
}
