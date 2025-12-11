#include <stdint.h>
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include <string.h>

#define CUDA_CHECK_ERROR(e)                                    \
do {                                                           \
  cudaError_t code = (e);                                      \
  if (code != cudaSuccess) {                                   \
    fprintf(stderr, "<%s:%d> %s:\n    %s: %s\n",               \
            __FILE__, __LINE__, #e,                            \
            cudaGetErrorName(code), cudaGetErrorString(code)); \
    fflush(stderr);                                            \
    exit(1);                                                   \
  }                                                            \
} while (0)

void* cuda_malloc_managed_i32(int64_t n) {
  int32_t* ptr;
  printf("cudaMallocManaged(..., %ld)\n", n * sizeof(int32_t));
  CUDA_CHECK_ERROR(cudaMallocManaged(&ptr, n * sizeof(int32_t), cudaMemAttachGlobal));
  printf("cudaMallocManaged: %p\n", (void*)ptr);
  return (void*)ptr;
}

void cuda_free(void* ptr) {
  CUDA_CHECK_ERROR(cudaFree(ptr));
}

void memcpy_i32_into_cuda(
  int32_t* a,
  int64_t a_start,
  int32_t* cuda_ptr,
  int64_t cuda_start,
  int64_t len)
{
  memcpy(cuda_ptr + cuda_start, a + a_start, len * sizeof(int32_t));
}

void memcpy_i32_from_cuda(
  int32_t* a,
  int64_t a_start,
  int32_t* cuda_ptr,
  int64_t cuda_start,
  int64_t len)
{
  memcpy(a + a_start, cuda_ptr + cuda_start, len * sizeof(int32_t));
}

int32_t* get_raw_ptr(int32_t* x, int64_t i) {
  return x + i;
}

int32_t check_raw_ptrs_equal(void* a, void* b) {
  printf("%p %p\n", a, b);
  return (a == b ? 1 : 0);
}