#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>

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

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  char* p = mmap(NULL, (size_t)(size + 2 * page),
                 PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANON,
                 0, 0);
  if (p == MAP_FAILED) {
    fprintf(stderr, "allocate_protected_space: mmap failed\n");
    exit(1);
  }
  status = mprotect(p, (size_t)page, PROT_NONE);
  if (status != 0) {
    fprintf(stderr, "allocate_protected_space: mprotect failed\n");
    exit(1);
  }
  status = mprotect(p + page + size, (size_t)page, PROT_NONE);
  if (status != 0) {
    fprintf(stderr, "allocate_protected_space: mprotect failed\n");
    exit(1);
  }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  status = munmap(p - page, (size_t)(size + 2 * page));
  if (status != 0) {
    fprintf(stderr, "deallocate_protected_space: nunmap failed\n");
    exit(1);
  }
}

int main() {
  int stack_size = 16 * 4096;
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
