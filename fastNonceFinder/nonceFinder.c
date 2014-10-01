
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "sha3.h"

#define NUMTHREADS 8

pthread_mutex_t     mutex = PTHREAD_MUTEX_INITIALIZER;

uint8_t savedNonce[32];
volatile bool nonceFound;

void saveNonce(uint8_t *val) {

  pthread_mutex_lock(&mutex);

  if (nonceFound) return; //Too late, you've been beaten to it

  memcpy(savedNonce, val, 32);

  nonceFound=true;

  pthread_mutex_unlock(&mutex);

}

struct ThreadData {
  int thread_num;
  uint8_t data[32];
  uint8_t threshold[32];
};

void getHash(uint8_t *in, uint8_t *out) {
  struct sha3_ctx ctx;
  sha3_init(&ctx, 256);
  sha3_update(&ctx, in, 64);
  sha3_finalize(&ctx, out);
}

int compare256(uint8_t first[32], uint8_t second[32]) {
  int i;
  for(i = 0; i < 32; i++) {
    if (first[i] < second[i]) return -1;
    else if (first[i] > second[i]) return 1;
  }
  return 0;
}

void increment256(uint8_t val[32]) {
  int i;
  for(i=31; i >= 0; i--) {
    val[i]++;
    if (val[i] != 0) return;
  }
}

void print256(uint8_t val[32]) {
  int i;
  for(i = 0; i < 32; i++)
    printf("%02x", val[i]);
  printf("\n");
}

void *calculateHashes(void *x) {
  uint8_t dataAndNonce[64] = {0};
  uint8_t out[32];

  struct ThreadData *threadData = (struct ThreadData *) x;

  printf("%d, #### starting thread\n", threadData->thread_num);
  

  memcpy(dataAndNonce, threadData->data, 32);

  uint64_t i;


  *(dataAndNonce + 32) = threadData->thread_num;
  while(true){
    //No mutex needed, because this is just a bool, it is only flipped to true in one place, and 
    //even if I read while writing, the worst case is that I calculate one extra sha hash.
    if (nonceFound) return NULL;
    increment256(dataAndNonce+32);
    getHash(dataAndNonce, out);
    if (compare256(out, threadData->threshold) == -1) {
      printf("data: ");
      print256(dataAndNonce);
      printf("nonce: ");
      print256(dataAndNonce+32);
      printf("out: ");
      print256(out);
      printf("done\n");
      saveNonce(dataAndNonce+32);
      return NULL;
    }
  }

  printf("shouldn't be here\n");

  exit(1);
}

int findNonce(uint8_t data[32], uint8_t threshold[32], uint8_t *out) {
  pthread_t inc_x_thread[NUMTHREADS];
  struct ThreadData threadData[NUMTHREADS];

  printf("data:      ");
  print256(data);
  printf("threshold: ");
  print256(threshold);

  int thread_num;

  int i;

  nonceFound=false;

  for(thread_num = 0; thread_num < NUMTHREADS-1; thread_num++) {
    
    memcpy(threadData[thread_num].threshold, threshold, 32);
    memcpy(threadData[thread_num].data, data, 32);
    threadData[thread_num].thread_num = thread_num;

    if(pthread_create(&inc_x_thread[thread_num], NULL, calculateHashes, (threadData+thread_num))) {
      fprintf(stderr, "Error creating thread\n");
      return 1;
    }
  }

  for(thread_num = 0; thread_num < NUMTHREADS-1; thread_num++) {
    if(pthread_join(inc_x_thread[thread_num], NULL)) {
      fprintf(stderr, "Error joining thread\n");
      return 2;
    }
  }

  assert(nonceFound);

  printf("savedNonce: ");
  print256(savedNonce);

  memcpy(out, savedNonce, 32);

  return 0;
  
}
