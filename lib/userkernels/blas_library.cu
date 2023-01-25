#include <stdio.h>
#include <time.h>
#include "../headers/export.h"
#include "cublas_v2.h"
#define SIZE 256
#define SHMEM_SIZE 256
// void * blasSGEMM(void * A, void* B, void * C, int m, int n, int k) {
//   float alpha = 1.0;
//   float beta = 0.0;
//   sgemm('N', 'N',  &n,  &m, &k, &alpha, B, &n, A, &k, &beta, C, &n);
//   return C;
// }

void * basicSGEMM(void * A, void* B, void * C, int m, int n, int k) {
    // float alpha = 1.0;
    // float beta = 0.0;
    // sgemm('N', 'N',  &n,  &m, &k, &alpha, B, &n, A, &k, &beta, C, &n);
    float * A2 = (float*)A;
    float * B2 = (float*)B;
    float * C2 = (float*)C;
    
    for(int i = 0; i < m; i++) {
      for(int j = 0; j< n; j++) {
        for(int l = 0; l < k; l++) {
          (C2[i*n+j]) += (A2[i*k+l]) * (B2[l*n+j]);
        }
      }
    }

    return C; 
  }

__global__ 
void tc(){
  
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
    printf("hello from thread %d\n", idx);
}


__global__
void set_flags(bool *flags, int numFlags, bool desired) {
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;
  for (int i = tid; i < numFlags; i += stride)
    flags[i] = desired;
}


__global__
void prime_sieve(int* primes, int numPrimes, bool* flags, int numFlags) {

  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;

  for (int i = 0; i < numPrimes; i++) {
    int p = primes[i];
    int numMultiples = (numFlags-1) / p - 1;
    for (int j = tid; j < numMultiples; j += stride) {
      flags[(j + 2) * p] = false; // mark as not prime
    }
  }
}

extern "C"
bool* do_prime_sieve(int* primes, int numPrimes, int numFlags) {
  bool *flagsOnDevice;
  cudaMallocManaged(&flagsOnDevice, numFlags * sizeof(bool));

  int *primesOnDevice;
  cudaMalloc(&primesOnDevice, numPrimes * sizeof(int));
  cudaMemcpy(primesOnDevice, primes, numPrimes * sizeof(int), cudaMemcpyHostToDevice);

  int GRID = (numFlags+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  set_flags<<<GRID, SIZE>>>(flagsOnDevice, numFlags, true);
  cudaDeviceSynchronize();

  /* seems okay to use same GRID here because parallelism of prime_sieve
   * kernel at the moment is only approximately ~ numFlags. Need to improve
   * this.
   */
  prime_sieve<<<GRID, SIZE>>>(primesOnDevice, numPrimes, flagsOnDevice, numFlags);
  cudaDeviceSynchronize();

  return flagsOnDevice;
}


__global__ void sum_reduction(int *v, int *v_r, int n) {
	// Allocate shared memory
	__shared__ int partial_sum[SHMEM_SIZE];

	// Calculate thread ID
	int tid = blockIdx.x * blockDim.x + threadIdx.x;
	// Load elements into shared memory
  if(tid < n)
	  partial_sum[threadIdx.x] = v[tid];
  else 
    partial_sum[threadIdx.x] = 0;
	__syncthreads();

	// Start at 1/2 block stride and divide by two each iteration
	for (int s = blockDim.x / 2; s > 0; s >>= 1) {
		// Each thread does work unless it is further than the stride
		if (threadIdx.x < s) {
			partial_sum[threadIdx.x] += partial_sum[threadIdx.x + s];
		}
		__syncthreads();
	}

	// Let the thread 0 for this block write it's result to main memory
	// Result is inexed by this block
	if (threadIdx.x == 0) {
		//v_r[blockIdx.x] = partial_sum[0];
    atomicAdd(v_r, partial_sum[threadIdx.x]);
	}
}

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
void * allocCudaManagedMemory(Int64 numBytes) {
  void *result;
  cudaMallocManaged(&result, (size_t)numBytes);
  return result;
}


extern "C"
void freeCudaMemory(void *p) {
  cudaFree(p);
}


extern "C"
int reduction_managed(int * A, int lo, int hi){
  // struct timespec t0, t1;
  // clock_gettime(CLOCK_MONOTONIC, &t0);

  int * result;
  cudaMalloc(&result, 1*sizeof(int));

  int n = hi - lo;
  int GRID = (n+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }

  sum_reduction<<<GRID, SIZE>>>(A+lo, result, n);
  cudaDeviceSynchronize();

  int h_result = 0;
  cudaMemcpy(&h_result, result, 1*sizeof(int), cudaMemcpyDeviceToHost);
  cudaFree(result);

  // clock_gettime(CLOCK_MONOTONIC, &t1);
  // report_elapsed("sum_reduction", &t1, &t0);

  return h_result;
}


extern "C"
int reduction(int * A, int lo, int hi){
  // for(int i = 0; i < (hi-lo); i++) {
  //   printf("gpu value %d ", A[i+lo]);
  // }
  // printf("\n");
  struct timespec t0, t1, t2, t3, t4;
  clock_gettime(CLOCK_MONOTONIC, &t0);
  int * A2;
  int n = hi - lo;
  // printf("%d\n", hi-lo);
  cudaMalloc(&A2, n *sizeof(int));
  // for(int i = 0; i < n; i++) {
  //   A2[i] = A[i];
  // }
  clock_gettime(CLOCK_MONOTONIC, &t1);
  report_elapsed("cudaMalloc", &t1, &t0);
  cudaMemcpy(A2, A+lo, n*sizeof(int), cudaMemcpyHostToDevice);
  clock_gettime(CLOCK_MONOTONIC, &t2);
  report_elapsed("cudaMemcpy", &t2, &t1);
  int * result;
  cudaMalloc(&result, 1*sizeof(int));
  // void* dev_ptr;
  // cudaMalloc(&dev_ptr, sizeof(float) * size);
  // int blockNum = (size / 256) + 1;

  int GRID = (n+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  // printf("input size %d, grid size %d, size %d\n", n, GRID, SIZE);
  sum_reduction<<<GRID, SIZE>>>(A2, result, n);
  cudaDeviceSynchronize();
  // printf("copying back\n");
  int h_result = 0;
  cudaMemcpy(&h_result, result, 1*sizeof(int), cudaMemcpyDeviceToHost);
  // printf("the gpu result is %d\n", h_result);

  clock_gettime(CLOCK_MONOTONIC, &t3);
  report_elapsed("sum_reduction", &t3, &t2);

  cudaFree(A2);
  cudaFree(result);

  clock_gettime(CLOCK_MONOTONIC, &t4);
  report_elapsed("cudaFree", &t4, &t3);

  return h_result;

}


extern "C"
void * cublasSGEMM(void* A, void* B, void* C, int m, int n, int k){
    
    cublasHandle_t handle;
    cublasCreate(&handle);
    float alpha = 1.0;
    float beta = 0.0;
    cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, m, k, &alpha, (float*) B, n, (float*) A, k, &beta, (float*) C, n);
    // cublasDestroy(handle);

    return C;
}

extern "C"
void * chooseSGEMM(void * A, void* B, void * C, int m, int n, int k) {
  //printf("hello from chooseSGEMM");
  if(k <= 100) {
    printf("entered cpu test\n");
    basicSGEMM(A, B, C, m, n, k);
  }
  else {
    printf("entered gpu test\n");
    //test_cuda();
    cublasSGEMM(A, B, C, m, n, k);
  }
  return C;
}


