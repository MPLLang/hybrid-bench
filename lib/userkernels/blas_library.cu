#include <stdio.h>
#include <time.h>
#include "../headers/export.h"
#include "cublas_v2.h"

extern "C"
void * cublasSGEMM(void* A, void* B, void* C, int m, int n, int k){
    
    cublasHandle_t handle;
    cublasCreate(&handle);
    float alpha = 1.0;
    float beta = 0.0;
    cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, m, n, k, &alpha, (float*) A, m, (float*) B, k, &beta, (float*) C, m);
    cublasDestroy(handle);

    return C;
}

__global__ 
void tc(){
  
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
    printf("hello from thread %d\n", idx);
}

extern "C"
void test_cuda(){
  
  
  // void* dev_ptr;
  // cudaMalloc(&dev_ptr, sizeof(float) * size);
  // int blockNum = (size / 256) + 1;
  tc<<<1, 10>>>();
  cudaDeviceSynchronize();

}