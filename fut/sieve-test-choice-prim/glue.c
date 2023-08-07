#include "sieve.h"
#include "timer.h"
#include <pthread.h>


// ==========================================================================
// context boilerplate


struct futStuff {
  struct futhark_context_config *cfg;
  struct futhark_context *ctx;
};

void* futInit() {
  struct timer_t t;
  timer_begin(&t, "futInit");

  struct futhark_context_config *cfg = futhark_context_config_new();
  timer_report_tick(&t, "futhark_context_config_new");

  struct futhark_context *ctx = futhark_context_new(cfg);
  timer_report_tick(&t, "futhark_context_new");

  struct futStuff *result = malloc(sizeof(struct futStuff));
  result->cfg = cfg;
  result->ctx = ctx;
  return (void *)result;
}

void futFinish(struct futStuff * futStuff) {
  struct futhark_context_config *cfg = futStuff->cfg;
  struct futhark_context *ctx = futStuff->ctx;

  struct timer_t t;
  timer_begin(&t, "futFinish");

  futhark_context_free(ctx);
  timer_report_tick(&t, "futhark_context_free");

  futhark_context_config_free(cfg);
  timer_report_tick(&t, "futhark_context_config_free");

  futStuff->ctx = NULL;
  futStuff->cfg = NULL;
  free(futStuff);
}


// ==========================================================================
// sieve boilerplate


struct sievePackage {
  struct futStuff *futStuff;
  struct futhark_i64_1d *input_arr;
  struct futhark_bool_1d *output_arr;
  int64_t outputLen;
  bool finished;
  pthread_t friend;
};


// run in separate pthread
void* asyncSieveFunc(void* rawArg) {
  struct timer_t t;
  timer_begin(&t, "asyncSieveFunc");

  struct sievePackage *pack = (struct sievePackage *)rawArg;

  futhark_entry_sieve(
    pack->futStuff->ctx,
    &(pack->output_arr),
    pack->input_arr,
    pack->outputLen);

  // timer_report_tick(&t, "futhark_entry_sieve");

  futhark_context_sync(pack->futStuff->ctx);

  timer_report_tick(&t, "done");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST);
  return NULL;
}


struct sievePackage *
futSieveSpawn(
  struct futStuff * futStuff,
  int64_t inputLen,
  int64_t* input,
  int64_t outputLen)
{
  // struct timer_t t;
  // timer_begin(&t, "futSieveSpawn");

  struct futhark_context *ctx = futStuff->ctx;

  struct sievePackage *pack = malloc(sizeof(struct sievePackage));
  pack->futStuff = futStuff;
  pack->input_arr = futhark_new_i64_1d(ctx, input, inputLen);
  pack->output_arr = NULL;
  pack->outputLen = outputLen;
  pack->finished = false;

  // timer_report_tick(&t, "initialize pack");

  if (0 != pthread_create(&(pack->friend), NULL, &asyncSieveFunc, pack)) {
    printf("ERROR: glue.c: futSieveSpawn: pthread_create failed\n");
    exit(1);
  }

  // timer_report_tick(&t, "spawn friend");
  return pack;
}


uint8_t futSievePoll(struct sievePackage *pack) {
  return pack->finished ? 1 : 0;
}


void futSieveFinish(
  struct sievePackage *pack,
  bool* output)
{
  // struct timer_t t;
  // timer_begin(&t, "futSieveFinish");

  if (0 != pthread_join(pack->friend, NULL)) {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }

  // timer_report_tick(&t, "pthread_join");

  futhark_values_bool_1d(pack->futStuff->ctx, pack->output_arr, output);
  futhark_free_i64_1d(pack->futStuff->ctx, pack->input_arr);
  futhark_free_bool_1d(pack->futStuff->ctx, pack->output_arr);
  free(pack);

  // timer_report_tick(&t, "cleanup");
}

// ==========================================================================
// primes boilerplate

struct primesPackage {
  struct futStuff *futStuff;

  int64_t n;
  struct futhark_i64_1d *output_arr;
  int64_t output_size;

  bool finished;
  pthread_t friend;
};


void* asyncPrimesFunc(void* rawArg) {
  struct primesPackage *pack = (struct primesPackage *)rawArg;

  struct timer_t t;
  timer_begin(&t, "asyncPrimesFunc");

  futhark_entry_primes(
    pack->futStuff->ctx,
    &(pack->output_arr),
    &(pack->output_size),
    pack->n
  );

  futhark_context_sync(pack->futStuff->ctx);

  timer_report_tick(&t, "done");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST);
  return NULL;
}


struct primesPackage *
futPrimesSpawn(
  struct futStuff * futStuff,
  int64_t n)
{
  // struct timer_t t;
  // timer_begin(&t, "futPrimesSpawn");

  struct primesPackage *pack = malloc(sizeof(struct primesPackage));
  pack->futStuff = futStuff;
  pack->n = n;
  pack->output_arr = NULL;
  pack->output_size = 0;
  pack->finished = false;

  if (0 != pthread_create(&(pack->friend), NULL, &asyncPrimesFunc, pack)) {
    printf("ERROR: glue.c: futPrimesSpawn: pthread_create failed\n");
    exit(1);
  }

  // timer_report_tick(&t, "done");

  return pack;
}


uint8_t futPrimesPoll(struct primesPackage *pack) {
  return pack->finished ? 1 : 0;
}


int64_t futPrimesOutputSize(struct primesPackage *pack) {
  // struct timer_t t;
  // timer_begin(&t, "futPrimesOutputSize");

  if (0 != pthread_join(pack->friend, NULL)) {
    printf("ERROR: glue.c: futPrimesOutputSize: pthread_join failed\n");
    exit(1);
  }

  // timer_report_tick(&t, "done");
  return pack->output_size;
}


void futPrimesFinish(
  struct sievePackage *pack,
  int64_t* output)
{
  // struct timer_t t;
  // timer_begin(&t, "futPrimesFinish");

  futhark_values_i64_1d(pack->futStuff->ctx, pack->output_arr, output);
  futhark_free_i64_1d(pack->futStuff->ctx, pack->output_arr);
  free(pack);

  // timer_report_tick(&t, "done");
}