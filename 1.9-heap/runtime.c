#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <inttypes.h>

/* define all scheme constants */
#define false_rep 0x2F
#define true_rep  0x6F
#define null_rep  0x3F

#define fixnum_mask 0b11
#define fixnum_tag  0b00
#define fixnum_shift 2

#define char_mask 0xFF
#define char_tag  0x0F
#define char_shift 8

#define pair_mask 0b111
#define pair_tag  0b001

#define vector_mask 0b111
#define vector_tag  0b101

#define string_mask 0b111
#define string_tag  0b110

#define word_size 8

#define vector_to_pointer(x) (ptr*)(x - vector_tag)
#define string_to_pointer(x) (ptr*)(x - string_tag)

/* all scheme values are of type ptrs */
typedef uint64_t ptr;

typedef struct {
  void* rbx;
  void* rbp;
  void* rsp;
} context_t;

extern ptr scheme_entry(context_t*, void*, void*);

static void print_ptr(ptr);
static void print_value(ptr);
static void print_value2(_Bool, ptr);
_Bool scheme_is_pair(ptr);

static void print_char(char c) {
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

static ptr scheme_car(ptr x) {
    ptr* car = (ptr*)(x - 1);
    return *car;
}

static ptr scheme_cdr(ptr x) {
    ptr* cdr = (ptr*)(x + 7);
    return *cdr;
}

static void print_pair(_Bool parens, ptr x) {
    if (parens)
      printf("(");
    print_value(scheme_car(x));
    ptr cdr = scheme_cdr(x);
    if (!scheme_is_pair(cdr)) {
        if (cdr != null_rep) {
            printf(" . ");
            print_value(cdr);
        }
        printf(")");
    } else {
        printf(" ");
        print_value2(0, cdr);
    }
}

static void print_vector(ptr x) {
  printf("#(");

  ptr* v = vector_to_pointer(x);
  uint64_t len = v[0] >> fixnum_shift;
  uint64_t i = 1;

  if (0 != len) {
      print_value(v[i]);
      i++;
      len--;
  }

  while (0 != len) {
    printf(" ");
    print_value(v[i]);
    i++;
    len--;
  }

  printf(")");
}

static void print_string(ptr x) {
    ptr* s = string_to_pointer(x);
    ptr* chars_start = s + 1;
    uint64_t len = s[0] >> fixnum_shift;
    char* chars = (char*)chars_start;

    printf("\"");

    for (uint64_t i = 0; i < len; ++i) {
        char c = chars[i];
        if (c == '\\') {
          printf("\\\\");
        } else if (c == '\n') {
          printf("\\n");
        } else if (c == '\"') {
          printf("\\\"");
        } else {
          printf("%c", (int)chars[i]);
        }
    }

    printf("\"");
}

_Bool scheme_is_pair(ptr x) {
  return ((x & pair_mask) == pair_tag);
}

_Bool scheme_is_vector(ptr x) {
  return ((x & vector_mask) == vector_tag);
}

_Bool scheme_is_string(ptr x) {
  return ((x & string_mask) == string_tag);
}

static void print_value(ptr x) {
  if((x & fixnum_mask) == fixnum_tag){
    printf("%d", ((int)x) >> fixnum_shift);
  } else if (x == false_rep){
    printf("#f");
  } else if (x == true_rep){
    printf("#t");
  } else if (x == null_rep){
    printf("()");
  } else if ((x & char_mask) == char_tag){
    print_char((char)((int)x >> char_shift));
  } else if (scheme_is_pair(x)){
    print_pair(1, x);
  } else if (scheme_is_vector(x)){
    print_vector(x);
  } else if (scheme_is_string(x)){
    print_string(x);
  } else {
    printf("#<unknown 0x%p>", (void*)x);
  }
}

static void print_value2(_Bool parens, ptr x) {
  if((x & fixnum_mask) == fixnum_tag){
    printf("%d", ((int)x) >> fixnum_shift);
  } else if (x == false_rep){
    printf("#f");
  } else if (x == true_rep){
    printf("#t");
  } else if (x == null_rep){
    printf("()");
  } else if ((x & char_mask) == char_tag){
    print_char((char)((int)x >> char_shift));
  } else if (scheme_is_pair(x)){
    print_pair(parens, x);
  } else if (scheme_is_vector(x)){
    print_vector(x);
  } else if (scheme_is_string(x)){
    print_string(x);
  } else {
    printf("#<unknown 0x%p>", (void*)x);
  }
}

static void print_ptr(ptr x){
  print_value(x);
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
  void* stack_top = allocate_protected_space(stack_size);
  void* stack_base = stack_top + stack_size;
  int heap_size = 16 * 4096;
  void* heap = allocate_protected_space(heap_size);
  context_t ctx;

  print_ptr(scheme_entry(&ctx, stack_base, heap));

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, heap_size);
  return 0;
}
