#include "../headers/export.h"
#include "../headers/hofs.h"

//Builtin function pointers
#include "../funcptrs/builtin_tabulate_and_map_float.h"
#include "../funcptrs/builtin_reduce_and_scan_float.h"
#include "../funcptrs/builtin_filter_float.h"

//User defined function pointers
#include "../funcptrs/user_tabulate_float.h"
#include "../funcptrs/user_map_float.h"
#include "../funcptrs/user_reduce_float.h"
#include "../funcptrs/user_scan_float.h"
#include "../funcptrs/user_filter_float.h"
#include "../funcptrs/user_zipwith_float.h"

#include <stdio.h>
#include <time.h>


extern "C"
void * cublasSGEMM(void* A, void* B, void* C, int m, int n, int k, void* f){
    cublasDGEMM_fun hof = (cublasDGEMM_fun)f;
    
    cublasHandle_t handle;
    cublasCreate(&handle);
    cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, m, n, k, 1, (float*) A, m, (float*) B, k, 0, (float*) C, m);
    cublasDestroy(handle);

    return C;
}

__global__ 
void test_cuda(float* arr, int len, test_cuda f){
  
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
    printf("hello from thread %d\n", idx);
}

extern "C"
void* test_cuda(int size, void* f){
  
  test_cuda hof = (test_cuda)f;
  
  void* dev_ptr;
  cudaMalloc(&dev_ptr, sizeof(float) * size);

  int blockNum = (size / 256) + 1;
  test_cuda<<<1, 10>>>((float*)dev_ptr, size, hof);
  cudaDeviceSynchronize();
  return dev_ptr;

}