#include <cblas.h>

/*
void cblas_sgemm(
  OPENBLAS_CONST enum CBLAS_ORDER Order,
  OPENBLAS_CONST enum CBLAS_TRANSPOSE TransA,
  OPENBLAS_CONST enum CBLAS_TRANSPOSE TransB,
  OPENBLAS_CONST blasint M,
  OPENBLAS_CONST blasint N,
  OPENBLAS_CONST blasint K,
  OPENBLAS_CONST float alpha,
  OPENBLAS_CONST float *A,
  OPENBLAS_CONST blasint lda,
  OPENBLAS_CONST float *B,
  OPENBLAS_CONST blasint ldb,
  OPENBLAS_CONST float beta,
  float *C,
  OPENBLAS_CONST blasint ldc);
*/

void cpu_sgemm(
  float* input1,   /* m * k */
  float* input2,   /* k * n */
  float* output,   /* m * n */
  int64_t m,
  int64_t n,
  int64_t k,
  int32_t bool_accumulate)
{
  if (bool_accumulate) {
    cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, 1.0, input1, k, input2, n, 1.0, output, n);
  }
  else {
    cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, 1.0, input1, k, input2, n, 0.0, output, n);
  }
}


void memcpy_floats(
  float* dst,
  int64_t dst_start,
  float* src,
  int64_t src_start,
  int64_t n)
{
  memcpy(dst+dst_start, src+src_start, n*sizeof(float));
}