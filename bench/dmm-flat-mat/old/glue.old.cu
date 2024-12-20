#include <time.h>
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include "cublas_v2.h"
#include <pthread.h>
#include <error.h>

#define SIZE 256

#define ENABLE_TIMER_TICKS false

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
  fflush(stdout);
}

void timer_begin(struct my_timer_t *t, const char *name) {
  t->name = name;
  clock_gettime(CLOCK_MONOTONIC, &(t->start));
  t->most_recent_tick = t->start;
}

void timer_report_tick(struct my_timer_t *t, const char *msg) {
  struct timespec prev = t->most_recent_tick;
  clock_gettime(CLOCK_MONOTONIC, &(t->most_recent_tick));
  if (ENABLE_TIMER_TICKS) {
    report_elapsed(t->name, msg, &(t->most_recent_tick), &prev);
  }
}

void set_cpu_affinity(int cpu) {
  cpu_set_t cpuset;
  pthread_t thread;

  thread = pthread_self();

  CPU_ZERO(&cpuset);
  CPU_SET(cpu, &cpuset);

  if (pthread_setaffinity_np(thread, sizeof cpuset, &cpuset) != 0) {
    printf("ERROR: glue.c: could not set affinity\n");
    exit(1);
  }
}

#define GPU_ID_BUFFER_SIZE 16

// static long parse_long(const char *str)
// {
//   errno = 0;
//   char *temp;
//   long val = strtol(str, &temp, 0);

//   if (temp == str || *temp != '\0' ||
//       ((val == LONG_MIN || val == LONG_MAX) && errno == ERANGE))
//   {
//     fprintf(stderr, "Could not convert '%s' to long and leftover string is: '%s'\n",
//             str, temp);
//     exit(1);
//   }
//   return val;
// }

int parse_cuda_device(
  char * gpu_id,
  int64_t gpu_id_str_len)
{
  char buf[GPU_ID_BUFFER_SIZE];
  if (gpu_id_str_len <= 0 || gpu_id_str_len > GPU_ID_BUFFER_SIZE || gpu_id[0] != '#') {
    printf("ERROR: glue.cu: parse_cuda_device: bad gpu_id\n");
    exit(1);
  }
  strncpy(&(buf[0]), gpu_id+1, gpu_id_str_len-1);
  buf[gpu_id_str_len-1] = '\0';
  char *end;
  long result = strtol(buf, &end, 10);
  if (end != buf + gpu_id_str_len-1) {
    printf("ERROR: glue.cu: parse_cuda_device: strtol failed\n");
    printf("buf contents: %s\n", buf);
    printf("result %ld\n", result);
    printf("buf %p gpu_id_str_len %ld end %p\n", (void*)buf, gpu_id_str_len, (void*)end);
    exit(1);
  }

  return (int)result;
}

void set_cuda_device(
  char * gpu_id,
  int64_t gpu_id_str_len)
{
  cudaSetDevice(parse_cuda_device(gpu_id, gpu_id_str_len));
}

// ==========================================================================

extern "C"
void * memcpyFloatsToGpu(
  char * gpu_id,
  int64_t gpu_id_str_len,
  float *data,
  int64_t len)
{
  struct my_timer_t t;
  timer_begin(&t, "memcpyFloatsToGpu");

  set_cuda_device(gpu_id, gpu_id_str_len);

  float *p;
  cudaMalloc(&p, len*sizeof(float));
  cudaMemcpyAsync(p, data, len*sizeof(float), cudaMemcpyHostToDevice);

  timer_report_tick(&t, "done");
  return p;
}

extern "C"
void synchronizeGpu(
  char * gpu_id,
  int64_t gpu_id_str_len)
{
  set_cuda_device(gpu_id, gpu_id_str_len);
  cudaDeviceSynchronize();
}

extern "C"
void freeFloatsOnGpu(
  char * gpu_id,
  int64_t gpu_id_str_len,
  void *devicePtr)
{
  set_cuda_device(gpu_id, gpu_id_str_len);
  cudaFree(devicePtr);
}


// ==========================================================================


// copy into dst[0..n*n)
__global__
void copy_block(
  float *dst,
  uint64_t height,
  uint64_t width,
  float *src,
  uint64_t top,
  uint64_t left,
  uint64_t rowskip)
{
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;

  int total = height*width;
  for (int i = tid; i < total; i += stride) {
    int row = i/width;
    int col = i%width;
    int srcIdx = (top + row) * rowskip + left + col;
    dst[i] = src[srcIdx];
  }
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
  int64_t m;
  int64_t n;
  int64_t k;

  /* these should stay */
  bool finished;
  pthread_t friends;
};



extern "C"
void* fancy_dmm_func(void* rawArg) {
  struct my_timer_t t;
  timer_begin(&t, "fancy_dmm_func");

  struct fancy_dmm_package *pack = (struct fancy_dmm_package *)rawArg;

  uint64_t m = pack->m;
  uint64_t n = pack->n;
  uint64_t k = pack->k;

  uint64_t abytes = m*k*sizeof(float);
  uint64_t bbytes = k*n*sizeof(float);
  uint64_t cbytes = m*n*sizeof(float);

  float *device_a;
  float *device_b;
  float *device_c;
  cudaMalloc(&device_a, abytes);
  cudaMalloc(&device_b, bbytes);
  cudaMalloc(&device_c, cbytes);

  int GRID = ((n*n)+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  copy_block<<<GRID, SIZE>>>(device_a, m, k, pack->a, pack->aTop, pack->aLeft, pack->aRowskip);
  // cudaDeviceSynchronize();

  copy_block<<<GRID, SIZE>>>(device_b, k, n, pack->b, pack->bTop, pack->bLeft, pack->bRowskip);
  cudaDeviceSynchronize();

  timer_report_tick(&t, "--- memcpy to gpu");

  float alpha = 1.0;
  float beta = 0.0;
  cublasHandle_t handle;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, m, n, k, &alpha, device_a, m, device_b, k, &beta, device_c, m);
  cublasDestroy(handle);
  timer_report_tick(&t, "      cublasSgemm");


  cudaMemcpy(pack->c, device_c, cbytes, cudaMemcpyDeviceToHost);

  cudaFree(device_a);
  cudaFree(device_b);
  cudaFree(device_c);
  timer_report_tick(&t, "  memcpy from gpu");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST); /* VERY IMPORTANT! */
  return NULL;
}


extern "C" struct fancy_dmm_package * 
fancy_dmm_spawn(
  char * gpu_id,
  int64_t gpu_id_str_len,
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
  int64_t m,
  int64_t n,
  int64_t k)
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

  pack->m = m;
  pack->n = n;
  pack->k = k;

  pack->finished = false;

  set_cuda_device(gpu_id, gpu_id_str_len);
  fancy_dmm_func(pack);

  // if (0 != pthread_create(&(pack->friends), NULL, &fancy_dmm_func, pack)) {
  //   printf("ERROR: glue.c: futdMMSpawn: pthread_create failed\n");
  //   exit(1);
  // }

  return pack;
}


/* TODO: memcpy from GPU back to pack->output
 *
 * (NOTE: futhark_values is equivalent of this memcpy. needs to be replaced) */
extern "C" void fancy_dmm_finish(
  struct fancy_dmm_package * pack)
{
  // if (0 != pthread_join(pack->friends, NULL)) {
  //   printf("ERROR: glue.c: pthread_join failed\n");
  //   exit(1);
  // }

  free(pack);
}


// ===========================================================================


struct fancy_two_dmm_package {
  float * a;  // on device
  int64_t aTop1;
  int64_t aLeft1;
  int64_t aTop2;
  int64_t aLeft2;
  int64_t aRowskip;
  float * b;  // on device
  int64_t bTop1;
  int64_t bLeft1;
  int64_t bTop2;
  int64_t bLeft2;
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



extern "C"
void* fancy_two_dmm_func(void* rawArg) {
  struct my_timer_t t;
  timer_begin(&t, "fancy_two_dmm_func");

  struct fancy_two_dmm_package *pack = (struct fancy_two_dmm_package *)rawArg;

  uint64_t n = pack->n;
  uint64_t rowbytes = n*sizeof(float);
  uint64_t bytes = n*rowbytes;


  float *device_c;
  cudaMalloc(&device_c, bytes);
  // for (int64_t j = 0; j < n; j++) {
  //   float *host_start = pack->c + (pack->cTop + j) * pack->cRowskip + pack->cLeft;
  //   cudaMemcpyAsync(device_c + j*n, host_start, rowbytes, cudaMemcpyHostToDevice);
  // }

  // cudaDeviceSynchronize();
  // timer_report_tick(&t, "----- memcpy C to gpu");

  float *tmp_a;
  float *tmp_b;
  cudaMalloc(&tmp_a, bytes);
  cudaMalloc(&tmp_b, bytes);


  int GRID = ((n*n)+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  copy_block<<<GRID, SIZE>>>(tmp_a, n, n, pack->a, pack->aTop1, pack->aLeft1, pack->aRowskip);
  copy_block<<<GRID, SIZE>>>(tmp_b, n, n, pack->b, pack->bTop1, pack->bLeft1, pack->bRowskip);
  cudaDeviceSynchronize();
  timer_report_tick(&t, "- memcpy A1,B1 on gpu");

  float alpha = 1.0;
  float beta = 0.0;
  cublasHandle_t handle;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, n, n, &alpha, tmp_a, n, tmp_b, n, &beta, device_c, n);
  cublasDestroy(handle);
  timer_report_tick(&t, "   cublasSgemm(A1,B1)");
  

  copy_block<<<GRID, SIZE>>>(tmp_a, n, n, pack->a, pack->aTop2, pack->aLeft2, pack->aRowskip);
  copy_block<<<GRID, SIZE>>>(tmp_b, n, n, pack->b, pack->bTop2, pack->bLeft2, pack->bRowskip);
  cudaDeviceSynchronize();
  timer_report_tick(&t, "  memcpy A2,B2 on gpu");


  beta = 1.0;
  cublasCreate(&handle);  
  cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, n, n, &alpha, tmp_a, n, tmp_b, n, &beta, device_c, n);
  cublasDestroy(handle);
  timer_report_tick(&t, "   cublasSgemm(A2,B2)");

  // for (int64_t j = 0; j < n; j++) {
  //   float *host_start = pack->c + (pack->cTop + j) * pack->cRowskip + pack->cLeft;
  //   cudaMemcpyAsync(host_start, device_c + j*n, rowbytes, cudaMemcpyDeviceToHost);
  // }
  // cudaDeviceSynchronize();

  cudaMemcpy(pack->c, device_c, bytes, cudaMemcpyDeviceToHost);
  cudaFree(tmp_a);
  cudaFree(tmp_b);
  cudaFree(device_c);
  timer_report_tick(&t, "    memcpy C from gpu");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST); /* VERY IMPORTANT! */
  return NULL;
}


extern "C" struct fancy_two_dmm_package * 
fancy_two_dmm_spawn(
  char * gpu_id,
  int64_t gpu_id_str_len,
  float * a,     // on device
  int64_t aTop1,
  int64_t aLeft1,
  int64_t aTop2,
  int64_t aLeft2,
  int64_t aRowskip,
  float * b,     // on device
  int64_t bTop1,
  int64_t bLeft1,
  int64_t bTop2,
  int64_t bLeft2,
  int64_t bRowskip,
  float * c,     // on host
  int64_t cTop,
  int64_t cLeft,
  int64_t cRowskip,
  int64_t n)
{
  struct fancy_two_dmm_package *pack = (fancy_two_dmm_package*)malloc(sizeof(struct fancy_two_dmm_package));

  pack->a = a;
  pack->aTop1 = aTop1;
  pack->aLeft1 = aLeft1;
  pack->aTop2 = aTop2;
  pack->aLeft2 = aLeft2;
  pack->aRowskip = aRowskip;

  pack->b = b;
  pack->bTop1 = bTop1;
  pack->bLeft1 = bLeft1;
  pack->bTop2 = bTop2;
  pack->bLeft2 = bLeft2;
  pack->bRowskip = bRowskip;

  pack->c = c;
  pack->cTop = cTop;
  pack->cLeft = cLeft;
  pack->cRowskip = cRowskip;

  pack->n = n;

  pack->finished = false;

  set_cuda_device(gpu_id, gpu_id_str_len);
  fancy_two_dmm_func(pack);

  // if (0 != pthread_create(&(pack->friends), NULL, &fancy_two_dmm_func, pack)) {
  //   printf("ERROR: glue.c: futdMMSpawn: pthread_create failed\n");
  //   exit(1);
  // }

  return pack;
}


// extern "C" uint8_t fancy_two_dmm_poll(struct fancy_two_dmm_package *pack) {
//   return pack->finished ? 1 : 0;
// }


extern "C" void fancy_two_dmm_finish(
  struct fancy_two_dmm_package * pack)
{
  // if (0 != pthread_join(pack->friends, NULL)) {
  //   printf("ERROR: glue.c: pthread_join failed\n");
  //   exit(1);
  // }

  free(pack);
}