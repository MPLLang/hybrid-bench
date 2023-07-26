#include <time.h>
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include "cublas_v2.h"
#include <pthread.h>

// ==========================================================================
// context boilerplate


/* TODO: this stuff can probably go away entirely */

// struct futStuff {
//   struct futhark_context_config *cfg;
//   struct futhark_context *ctx;
// };

// void* futInit() {
//   struct timer_t t;
//   timer_begin(&t, "futInit");

//   struct futhark_context_config *cfg = futhark_context_config_new();
//   timer_report_tick(&t, "futhark_context_config_new");

//   struct futhark_context *ctx = futhark_context_new(cfg);
//   timer_report_tick(&t, "futhark_context_new");

//   struct futStuff *result = malloc(sizeof(struct futStuff));
//   result->cfg = cfg;
//   result->ctx = ctx;
//   return (void *)result;
// }

// void futFinish(struct futStuff * futStuff) {
//   struct futhark_context_config *cfg = futStuff->cfg;
//   struct futhark_context *ctx = futStuff->ctx;

//   struct timer_t t;
//   timer_begin(&t, "futFinish");

//   futhark_context_free(ctx);
//   timer_report_tick(&t, "futhark_context_free");

//   futhark_context_config_free(cfg);
//   timer_report_tick(&t, "futhark_context_config_free");

//   futStuff->ctx = NULL;
//   futStuff->cfg = NULL;
//   free(futStuff);
// }

// ==========================================================================
// timer stuff

struct my_timer_t {
  const char *name;
  struct timespec start;
  struct timespec most_recent_tick;
};

static void timespec_subtract(struct timespec *x, struct timespec *y) {
  if (x->tv_nsec < y->tv_nsec) {
    x->tv_sec -= 1;
    x->tv_nsec += 1000000000L;
  }
  x->tv_sec -= y->tv_sec;
  x->tv_nsec -= y->tv_nsec;
}

static void report_elapsed(
  const char *name, 
  const char *msg, 
  struct timespec *x, 
  struct timespec *y)
{
  struct timespec diff = *x;
  timespec_subtract(&diff, y);
  double secs = (double)diff.tv_sec + ((double)diff.tv_nsec / 1000000000.0);
  printf("tick: %s: %s: elapsed: %lf\n", name, msg, secs);
}

void timer_begin(struct my_timer_t *t, const char *name) {
  t->name = name;
  clock_gettime(CLOCK_MONOTONIC, &(t->start));
  t->most_recent_tick = t->start;
}

void timer_report_tick(struct my_timer_t *t, const char *msg) {
  struct timespec prev = t->most_recent_tick;
  clock_gettime(CLOCK_MONOTONIC, &(t->most_recent_tick));
  report_elapsed(t->name, msg, &(t->most_recent_tick), &prev);
}


// ==========================================================================
// dMM boilerplate


/* TODO: inputs and outputs for leaf DMM, dimension info, etc. */
struct dMMPackage {
  // struct futStuff *futStuff;  /* won't need this */

  /* need to be specialized for DMM */
  float * a;
  float * b;
  float * output;
  uint64_t inputLen;

  /* these should stay */
  bool finished;
  pthread_t friends;
};

/* TODO: call cublas */
void* asyncdMMFunc(void* rawArg) {
  struct my_timer_t t;
  timer_begin(&t, "asyncdMMFunc");

  struct dMMPackage *pack = (struct dMMPackage *)rawArg;

  float *device_a;
  float *device_b;
  float *device_output;
  uint64_t n = pack->inputLen;

  cudaMalloc(&device_a, n*n*sizeof(float));
  cudaMemcpy(device_a, (float*)pack->a, n*n*sizeof(float), cudaMemcpyHostToDevice);


  cudaMalloc(&device_b,  n*n*sizeof(float));
  cudaMemcpy(device_b, (float*)pack->b,  n*n*sizeof(float), cudaMemcpyHostToDevice);
  
  cudaMalloc(&(device_output),  n*n*sizeof(float));

  float alpha = 1.0;
  float beta = 0.0;
  cublasHandle_t handle;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, n, n, &alpha, device_a, n, device_b, n, &beta, device_output, n);
  cublasDestroy(handle);

  cudaMemcpy(pack->output, device_output, pack->inputLen*sizeof(float), cudaMemcpyDeviceToHost);
  cudaFree(device_a);
  cudaFree(device_b);
  cudaFree(device_output);

  timer_report_tick(&t, "done");
  pack->finished = true; /* VERY IMPORTANT! */
  return NULL;
}


/* TODO: build the package, but otherwise shouldn't need to change much. 
 *
 * (NOTE: futhark_new_... is essentially a memcpy, these need to be replaced
 *  with stuff for cublas)
 */
extern "C" struct dMMPackage * 
dMMSpawn(
  float * a,
  float * b,
  float * output,
  int64_t inputLen)
{
  // struct futhark_context *ctx = futStuff->ctx;
  struct dMMPackage *pack = (dMMPackage*)malloc(sizeof(struct dMMPackage));
  // pack->futStuff = futStuff;
  // pack->a = futhark_new_u8_1d(ctx, a, inputLen);

  pack->a = a;
  pack->b = b;
  pack->output = output;
  pack->inputLen = inputLen;
  pack->finished = false;

  if (0 != pthread_create(&(pack->friends), NULL, &asyncdMMFunc, pack)) {
    printf("ERROR: glue.c: futdMMSpawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}

/* TODO: probably doesn't need to change */
extern "C" uint8_t dMMPoll(struct dMMPackage *pack) {
  return pack->finished ? 1 : 0;
}

// int64_t futBigAddOutputSize(struct bigAddPackage *pack) {
//   // struct timer_t t;
//   // timer_begin(&t, "futPrimesOutputSize");

//   if (0 != pthread_join(pack->friend, NULL)) {
//     printf("ERROR: glue.c: futBigAddOutputSize: pthread_join failed\n");
//     exit(1);
//   }

//   // timer_report_tick(&t, "done");
//   return pack->outputLen;
// }

/* TODO: memcpy from GPU back to pack->output
 *
 * (NOTE: futhark_values is equivalent of this memcpy. needs to be replaced) */
extern "C" void dMMFinish(
  struct dMMPackage * pack)
{
  if (0 != pthread_join(pack->friends, NULL)) {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }

  free(pack);
}
