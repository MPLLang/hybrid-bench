#include <time.h>
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include "cublas_v2.h"
#include <pthread.h>
#define SIZE 256

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
  uint64_t bytes = n*n*sizeof(float);

  cudaMalloc(&device_a, bytes);
  cudaMemcpy(device_a, pack->a, bytes, cudaMemcpyHostToDevice);

  cudaMalloc(&device_b, bytes);
  cudaMemcpy(device_b, pack->b, bytes, cudaMemcpyHostToDevice);
  
  cudaMalloc(&(device_output), bytes);
  cudaMemcpy(device_output, pack->output, bytes, cudaMemcpyHostToDevice);
  // timer_report_tick(&t, "--- memcpy to gpu");

  float alpha = 1.0;
  float beta = 1.0;
  cublasHandle_t handle;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, n, n, &alpha, device_a, n, device_b, n, &beta, device_output, n);
  cublasDestroy(handle);
  // timer_report_tick(&t, "      cublasSgemm");

  cudaMemcpy(pack->output, device_output, bytes, cudaMemcpyDeviceToHost);
  cudaFree(device_a);
  cudaFree(device_b);
  cudaFree(device_output);
  // timer_report_tick(&t, "  memcpy from gpu");

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

extern "C" uint8_t dMMPoll(struct dMMPackage *pack) {
  return pack->finished ? 1 : 0;
}


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

// ==========================================================================

extern "C"
void * memcpyFloatsToGpu(float *data, int64_t len) {
  struct my_timer_t t;
  timer_begin(&t, "memcpyFloatsToGpu");

  float *p;
  cudaMalloc(&p, len*sizeof(float));
  cudaMemcpyAsync(p, data, len*sizeof(float), cudaMemcpyHostToDevice);

  timer_report_tick(&t, "done");
  return p;
}

extern "C"
void synchronizeGpu() {
  cudaDeviceSynchronize();
}

extern "C"
void freeFloatsOnGpu(void *devicePtr) {
  cudaFree(devicePtr);
}


// ==========================================================================


struct fancy_dmm_package {
  float * a;  // on device
  int64_t aTop;
  int64_t aLeft;
  int64_t aRowskip;
  float * b;  // on device
  int64_t bTop;
  int64_t bLeft;
  int64_t bRowskip;
  float * c;  // on host
  int64_t cTop;
  int64_t cLeft;
  int64_t cRowskip;
  int64_t n;

  /* these should stay */
  bool finished;
  pthread_t friends;
};


// copy into dst[0..n*n)
__global__
void copy_square_block(
  float *dst,
  uint64_t n,
  float *src,
  uint64_t top,
  uint64_t left,
  uint64_t rowskip)
{
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;

  int total = n*n;
  for (int i = tid; i < total; i += stride) {
    int row = i/n;
    int col = i%n;
    int srcIdx = (top + row) * rowskip + left + col;
    dst[i] = src[srcIdx];
  }
}


extern "C"
void* fancy_dmm_func(void* rawArg) {
  struct my_timer_t t;
  timer_begin(&t, "fancy_dmm_func");

  struct fancy_dmm_package *pack = (struct fancy_dmm_package *)rawArg;

  uint64_t n = pack->n;
  uint64_t rowbytes = n*sizeof(float);
  uint64_t bytes = n*rowbytes;


  float *device_a;
  // float *tmp_a = (float*)malloc(bytes);
  cudaMalloc(&device_a, bytes);
  /*
  for (int64_t j = 0; j < n; j++) {
    float *host_start = pack->a + (pack->aTop + j) * pack->aRowskip + pack->aLeft;
    cudaMemcpyAsync(device_a + j*n, host_start, rowbytes, cudaMemcpyDeviceToDevice);
    // memcpy(tmp_a + j*n, host_start, rowbytes);
  }
  */
  // cudaMemcpy(device_a, tmp_a, bytes, cudaMemcpyHostToDevice);
  // free(tmp_a);


  float *device_b;
  // float *tmp_b = (float*)malloc(bytes);
  cudaMalloc(&device_b, bytes);
  /*
  for (int64_t j = 0; j < n; j++) {
    float *host_start = pack->b + (pack->bTop + j) * pack->bRowskip + pack->bLeft;
    cudaMemcpyAsync(device_b + j*n, host_start, rowbytes, cudaMemcpyDeviceToDevice);
    // memcpy(tmp_b + j*n, host_start, rowbytes);
  }
  */
  // cudaMemcpy(device_b, tmp_b, bytes, cudaMemcpyHostToDevice);
  // free(tmp_b);


  float *device_c;
  // float *tmp_c = (float*)malloc(bytes);
  cudaMalloc(&device_c, bytes);
  for (int64_t j = 0; j < n; j++) {
    float *host_start = pack->c + (pack->cTop + j) * pack->cRowskip + pack->cLeft;
    cudaMemcpyAsync(device_c + j*n, host_start, rowbytes, cudaMemcpyHostToDevice);
    // memcpy(tmp_c + j*n, host_start, rowbytes);
  }
  // cudaMemcpy(device_c, tmp_c, bytes, cudaMemcpyHostToDevice);
  // free(tmp_c);

  int GRID = ((n*n)+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  copy_square_block<<<GRID, SIZE>>>(device_a, n, pack->a, pack->aTop, pack->aLeft, pack->aRowskip);
  // cudaDeviceSynchronize();

  copy_square_block<<<GRID, SIZE>>>(device_b, n, pack->b, pack->bTop, pack->bLeft, pack->bRowskip);
  cudaDeviceSynchronize();

  timer_report_tick(&t, "--- memcpy to gpu");

  float alpha = 1.0;
  float beta = 1.0;
  cublasHandle_t handle;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, n, n, &alpha, device_a, n, device_b, n, &beta, device_c, n);
  cublasDestroy(handle);
  timer_report_tick(&t, "      cublasSgemm");


  // float *tmp_c = (float*)malloc(bytes);
  // cudaMemcpy(tmp_c, device_c, bytes, cudaMemcpyDeviceToHost);

  for (int64_t j = 0; j < n; j++) {
    float *host_start = pack->c + (pack->cTop + j) * pack->cRowskip + pack->cLeft;
    // memcpy(host_start, tmp_c + j*n, rowbytes);
    cudaMemcpyAsync(host_start, device_c + j*n, rowbytes, cudaMemcpyDeviceToHost);
  }
  // free(tmp_c);
  cudaDeviceSynchronize();

  cudaFree(device_a);
  cudaFree(device_b);
  cudaFree(device_c);
  timer_report_tick(&t, "  memcpy from gpu");

  pack->finished = true; /* VERY IMPORTANT! */
  return NULL;
}


extern "C" struct fancy_dmm_package * 
fancy_dmm_spawn(
  float * a,     // on device
  int64_t aTop,
  int64_t aLeft,
  int64_t aRowskip,
  float * b,     // on device
  int64_t bTop,
  int64_t bLeft,
  int64_t bRowskip,
  float * c,     // on host
  int64_t cTop,
  int64_t cLeft,
  int64_t cRowskip,
  int64_t n)
{
  struct fancy_dmm_package *pack = (fancy_dmm_package*)malloc(sizeof(struct fancy_dmm_package));

  pack->a = a;
  pack->aTop = aTop;
  pack->aLeft = aLeft;
  pack->aRowskip = aRowskip;

  pack->b = b;
  pack->bTop = bTop;
  pack->bLeft = bLeft;
  pack->bRowskip = bRowskip;

  pack->c = c;
  pack->cTop = cTop;
  pack->cLeft = cLeft;
  pack->cRowskip = cRowskip;

  pack->n = n;

  pack->finished = false;
  if (0 != pthread_create(&(pack->friends), NULL, &fancy_dmm_func, pack)) {
    printf("ERROR: glue.c: futdMMSpawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}


extern "C" uint8_t fancy_dmm_poll(struct fancy_dmm_package *pack) {
  return pack->finished ? 1 : 0;
}


/* TODO: memcpy from GPU back to pack->output
 *
 * (NOTE: futhark_values is equivalent of this memcpy. needs to be replaced) */
extern "C" void fancy_dmm_finish(
  struct fancy_dmm_package * pack)
{
  if (0 != pthread_join(pack->friends, NULL)) {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }

  free(pack);
}