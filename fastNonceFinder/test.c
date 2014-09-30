
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "nonceFinder.c"


void usage() {
  printf("Usage:\n");
  printf("  test <256 bit data> <256 bit threshold>\n");
  exit(1);
}

int main(int argc, char **argv) {

  uint8_t out[32];

  if (argc != 3) usage();

  if (strlen(argv[1]) != 64) usage();
  if (strlen(argv[2]) != 64) usage();

  uint8_t globalThreshold[32];
  uint8_t globalData[32];

  int i;
  char *p1 = argv[1];
  char *p2 = argv[2];

  for(i = 0; i < 32; i++) {
    sscanf(p1, "%2hhx", &globalData[i]);
    sscanf(p2, "%2hhx", &globalThreshold[i]);
    p1 += 2;
    p2 += 2;
  }

  findNonce(globalData, globalThreshold, out);

  print256(out);

  return 0;
  
}
