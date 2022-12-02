#include <stdio.h>
#include <time.h>
#include "../headers/export.h"
#include "cublas_v2.h"

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

extern "C"
void test_cuda(){
  
  
  // void* dev_ptr;
  // cudaMalloc(&dev_ptr, sizeof(float) * size);
  // int blockNum = (size / 256) + 1;
  tc<<<1, 10>>>();
  cudaDeviceSynchronize();

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


