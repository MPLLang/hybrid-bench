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
  float* input1,
  float* input2,
  float* output,
  int64_t n)
{
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasTrans, n, n, n, 1.0, input1, n, input2, n, 1.0, output, n);
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