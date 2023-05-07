#include <stdio.h>
#include <time.h>
#include "cublas_v2.h"

extern "C"
void timespec_subtract(struct timespec *x, struct timespec *y) {
  if (x->tv_nsec < y->tv_nsec) {
    x->tv_sec -= 1;
    x->tv_nsec += 1000000000L;
  }
  x->tv_sec -= y->tv_sec;
  x->tv_nsec -= y->tv_nsec;
}

extern "C"
void report_elapsed(const char *msg, struct timespec *x, struct timespec *y) {
  struct timespec diff = *x;
  timespec_subtract(&diff, y);
  double secs = (double)diff.tv_sec + ((double)diff.tv_nsec / 1000000000.0);
  printf("|%s: elapsed: %lf\n", msg, secs);
}

extern "C"
void * cublasSGEMM(void* A, void* B, void* C, int m, int n, int k){
    
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    cublasHandle_t handle;
    cublasCreate(&handle);
    float alpha = 1.0;
    float beta = 0.0;
    cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, m, k, &alpha, (float*) B, n, (float*) A, k, &beta, (float*) C, n);
    clock_gettime(CLOCK_MONOTONIC, &t1);
    report_elapsed("cublasMxM", &t1, &t0);
    // cublasDestroy(handle);

    return C;
}



