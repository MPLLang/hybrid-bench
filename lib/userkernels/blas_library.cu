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
int test_cuda(int * A, int n){
  
  // for(int i = 0; i < n; i++) {
  //   printf("gpu value %d ", A[i]);
  // }
  // printf("\n");
  int * A2;
  cudaMallocManaged(&A2, n *sizeof(int));
  for(int i = 0; i < n; i++) {
    A2[i] = A[i];
  }
  int * result;
  cudaMallocManaged(&result, 1*sizeof(int));
  // void* dev_ptr;
  // cudaMalloc(&dev_ptr, sizeof(float) * size);
  // int blockNum = (size / 256) + 1;

  int GRID = (n+(SIZE-1))/SIZE;
  if(GRID == 0) {
    GRID = 1;
  }
  printf("input size %d, grid size %d, size %d\n", n, GRID, SIZE);
  sum_reduction<<<GRID, SIZE>>>(A2, result, n);
  cudaDeviceSynchronize();
  printf("the result is %d\n", result[0]);
  return result[0];

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


