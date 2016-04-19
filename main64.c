
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern void error() asm("error");
extern int64_t print(int64_t val) asm("print");

int64_t print(int64_t val) {
  if(val & 1 ^ 1) {
    printf("%lld\n", val >> 1);
  }
  else if(val == 0x1f) {
    printf("true\n");
  }
  else if(val == 0xf) {
    printf("false\n");
  }
  else {
    printf("Unknown value: %#18llx\n", val);
  }
  return val;
}

void error(int64_t i) {
  if (i == 0) {
    fprintf(stderr, "Error: comparison operator got boolean");
  }
  else if (i == 1) {
    fprintf(stderr, "Error: arithmetic operator got boolean");
  }
  else if (i == 2) {
    fprintf(stderr, "Error: if condition got number, expected a boolean");
  }
  else if (i == 3) {
    fprintf(stderr, "Error: Integer overflow");
  }
  else {
    fprintf(stderr, "Error: Unknown error code: %lld\n", i);
  }
  exit(i);
}

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  print(result);
  return 0;
}
