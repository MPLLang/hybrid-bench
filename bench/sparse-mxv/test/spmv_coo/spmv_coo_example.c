/*
 * Copyright 1993-2022 NVIDIA Corporation.  All rights reserved.
 *
 * NOTICE TO LICENSEE:
 *
 * This source code and/or documentation ("Licensed Deliverables") are
 * subject to NVIDIA intellectual property rights under U.S. and
 * international Copyright laws.
 *
 * These Licensed Deliverables contained herein is PROPRIETARY and
 * CONFIDENTIAL to NVIDIA and is being provided under the terms and
 * conditions of a form of NVIDIA software license agreement by and
 * between NVIDIA and Licensee ("License Agreement") or electronically
 * accepted by Licensee.  Notwithstanding any terms or conditions to
 * the contrary in the License Agreement, reproduction or disclosure
 * of the Licensed Deliverables to any third party without the express
 * written consent of NVIDIA is prohibited.
 *
 * NOTWITHSTANDING ANY TERMS OR CONDITIONS TO THE CONTRARY IN THE
 * LICENSE AGREEMENT, NVIDIA MAKES NO REPRESENTATION ABOUT THE
 * SUITABILITY OF THESE LICENSED DELIVERABLES FOR ANY PURPOSE.  IT IS
 * PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY OF ANY KIND.
 * NVIDIA DISCLAIMS ALL WARRANTIES WITH REGARD TO THESE LICENSED
 * DELIVERABLES, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,
 * NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.
 * NOTWITHSTANDING ANY TERMS OR CONDITIONS TO THE CONTRARY IN THE
 * LICENSE AGREEMENT, IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY
 * SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, OR ANY
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THESE LICENSED DELIVERABLES.
 *
 * U.S. Government End Users.  These Licensed Deliverables are a
 * "commercial item" as that term is defined at 48 C.F.R. 2.101 (OCT
 * 1995), consisting of "commercial computer software" and "commercial
 * computer software documentation" as such terms are used in 48
 * C.F.R. 12.212 (SEPT 1995) and is provided to the U.S. Government
 * only as a commercial end item.  Consistent with 48 C.F.R.12.212 and
 * 48 C.F.R. 227.7202-1 through 227.7202-4 (JUNE 1995), all
 * U.S. Government End Users acquire the Licensed Deliverables with
 * only those rights set forth herein.
 *
 * Any use of the Licensed Deliverables in individual and commercial
 * software must include, in the user documentation and internal
 * comments to the code, the above Disclaimer and U.S. Government End
 * Users Notice.
 */
#include <cuda_runtime_api.h> // cudaMalloc, cudaMemcpy, etc.
#include <cusparse.h>         // cusparseSpMV
#include <stdio.h>            // printf
#include <stdlib.h>           // EXIT_FAILURE
#include <inttypes.h>

// #define CHECK_CUDA(func)                                                       \
// {                                                                              \
//     cudaError_t status = (func);                                               \
//     if (status != cudaSuccess) {                                               \
//         printf("CUDA API failed at line %d with error: %s (%d)\n",             \
//                __LINE__, cudaGetErrorString(status), status);                  \
//         return EXIT_FAILURE;                                                   \
//     }                                                                          \
// }

// #define CHECK_CUSPARSE(func)                                                   \
// {                                                                              \
//     cusparseStatus_t status = (func);                                          \
//     if (status != CUSPARSE_STATUS_SUCCESS) {                                   \
//         printf("CUSPARSE API failed at line %d with error: %s (%d)\n",         \
//                __LINE__, cusparseGetErrorString(status), status);              \
//         return EXIT_FAILURE;                                                   \
//     }                                                                          \
// }

#define CHECK_CUDA(func) { (func); }
#define CHECK_CUSPARSE(func) { (func); }

int main(void) {
    // Host problem definition
    int   A_num_rows   = 4;
    int   A_num_rows2  = 4;
    int   A_num_cols   = 4;
    int   A_nnz1        = 4;
    int   A_nnz2        = 5;
    int   hA_rows[]    = { 0, 0, 0, 1};
    int   hA_rows2[]    = { 2, 2, 2, 3, 3 };
    int   hA_columns[] = { 0, 2, 3, 1};
    int   hA_columns2[] = { 0, 2, 3, 1, 3 };
    float hA_values[]  = { 1.0f, 2.0f, 3.0f, 4.0f };
    float hA_values2[]  = {5.0f, 6.0f, 7.0f, 8.0f, 9.0f };
    float hX[]         = { 1.0f, 2.0f, 3.0f, 4.0f };
    float hY[]         = { 0.0f, 0.0f, 0.0f, 0.0f };
    float hY_result[]  = { 19.0f, 8.0f, 51.0f, 52.0f };
    float alpha        = 1.0f;
    float beta         = 0.0f;
    //--------------------------------------------------------------------------
    // Device memory management
    int   *dA_rows, *dA_rows2, *dA_columns, *dA_columns2;
    float *dA_values, *dA_values2, *dX, *dY;
    CHECK_CUDA( cudaMalloc((void**) &dA_rows,    A_nnz1 * sizeof(int))        )
    CHECK_CUDA( cudaMalloc((void**) &dA_rows2,    A_nnz2 * sizeof(int))        )
    CHECK_CUDA( cudaMalloc((void**) &dA_columns, A_nnz1 * sizeof(int))        )
    CHECK_CUDA( cudaMalloc((void**) &dA_columns2, A_nnz2 * sizeof(int))        )
    CHECK_CUDA( cudaMalloc((void**) &dA_values,  A_nnz1 * sizeof(float))      )
    CHECK_CUDA( cudaMalloc((void**) &dA_values2,  A_nnz2 * sizeof(float))      )
    CHECK_CUDA( cudaMalloc((void**) &dX,         A_num_cols * sizeof(float)) )
    CHECK_CUDA( cudaMalloc((void**) &dY,         A_num_rows * sizeof(float)) )

    CHECK_CUDA( cudaMemcpy(dA_rows, hA_rows, A_nnz1 * sizeof(int),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dA_rows2, hA_rows2, A_nnz2 * sizeof(int),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dA_columns, hA_columns, A_nnz1 * sizeof(int),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dA_columns2, hA_columns2, A_nnz2 * sizeof(int),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dA_values, hA_values, A_nnz1 * sizeof(float),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dA_values2, hA_values2, A_nnz2 * sizeof(float),
                           cudaMemcpyHostToDevice) )
    CHECK_CUDA( cudaMemcpy(dX, hX, A_num_cols * sizeof(float),
                           cudaMemcpyHostToDevice) )
    // CHECK_CUDA( cudaMemcpy(dY, hY, A_num_rows2 * sizeof(float),
    //                        cudaMemcpyHostToDevice) )
    printf("performing cuda memset\n");
    CHECK_CUDA( cudaMemset(dY, 0, A_num_rows2 * sizeof(float)) )
    //--------------------------------------------------------------------------
    // CUSPARSE APIs
    cusparseHandle_t     handle = NULL;
    cusparseMatDescr_t matA;
    cusparseMatDescr_t vecX, vecY;
    void*                dBuffer    = NULL;
    size_t               bufferSize = 0;
    CHECK_CUSPARSE( cusparseCreate(&handle) )

    cusparseCreateMatDescr(&matA);
    cusparseSetMatIndexBase(matA, CUSPARSE_INDEX_BASE_ZERO);
    cusparseSetMatType(matA, CUSPARSE_MATRIX_TYPE_GENERAL);

    // Create sparse matrix A in CSR format

    // CHECK_CUSPARSE( cusparseCreateCoo(&matA, A_num_rows2, A_num_cols, A_nnz1,
    //                                   dA_rows, dA_columns, dA_values,
    //                                   CUSPARSE_INDEX_64I,
    //                                   CUSPARSE_INDEX_BASE_ZERO, CUDA_R_32F) )
    // // Create dense vector X
    // CHECK_CUSPARSE( cusparseCreateDnVec(&vecX, A_num_cols, dX, CUDA_R_32F) )
    // // Create dense vector y
    // CHECK_CUSPARSE( cusparseCreateDnVec(&vecY, A_num_rows2, dY, CUDA_R_32F) )
    // // allocate an external buffer if needed
    // CHECK_CUSPARSE( cusparseSpMV_bufferSize(
    //                              handle, CUSPARSE_OPERATION_NON_TRANSPOSE,
    //                              &alpha, matA, vecX, &beta, vecY, CUDA_R_32F,
    //                              CUSPARSE_SPMV_ALG_DEFAULT, &bufferSize) )

    // CHECK_CUDA( cudaMalloc(&dBuffer, bufferSize) )

    // execute SpMV
    // CHECK_CUSPARSE( cusparseSpMV(handle, CUSPARSE_OPERATION_NON_TRANSPOSE,
    //                              &alpha, matA, vecX, &beta, vecY, CUDA_R_32F,
    //                              CUSPARSE_SPMV_ALG_DEFAULT, dBuffer) )

    // cusparseScoomv();

    // cusparseCsrmvEx(cusparseHandle_t handle, 
    //                 cusparseAlgMode_t alg,
    //                 cusparseOperation_t transA, 
    //                 int m, int n, int nnz,
    //                 const void *alpha, cudaDataType alphatype,
    //                 const cusparseMatDescr_t descrA,
    //                 const void *csrValA, cudaDataType csrValAtype,
    //                 const int *csrRowPtrA,
    //                 const int *csrColIndA,
    //                 const void *x, cudaDataType xtype,
    //                 const void *beta, cudaDataType betatype,
    //                 void *y, cudaDataType ytype,
    //                 cudaDataType executiontype,
    //                 void* buffer);

    int *    A_csrRowPtr=0;
    /* exercise conversion routines (convert matrix from COO 2 CSR format) */
    cudaMalloc(&A_csrRowPtr, (A_num_rows+1)*sizeof(A_csrRowPtr[0]));

    cusparseXcoo2csr(handle, dA_rows, A_nnz1, A_num_rows,
                     A_csrRowPtr, CUSPARSE_INDEX_BASE_ZERO); 


    cusparseScsrmv(handle, CUSPARSE_OPERATION_NON_TRANSPOSE,
      A_num_rows, A_num_cols, A_nnz1, &alpha, matA, dA_values, A_csrRowPtr, 
      dA_columns, dX, &beta, dY);

    /* dY is output */

    // -----------------------------------------------------------------------


    // copy dY -> (host) hY
    // and then print to see result
    CHECK_CUDA( cudaMemcpy(hY, dY, A_num_rows2 * sizeof(float),
                           cudaMemcpyDeviceToHost) )

    printf("%lf, %lf, %lf, %lf\n", hY[0], hY[1], hY[2], hY[3]);
    
    // destroy matrix/vector descriptors
    // CHECK_CUSPARSE( cusparseDestroySpMat(matA) )
    // CHECK_CUSPARSE( cusparseDestroyDnVec(vecX) )
    // CHECK_CUSPARSE( cusparseDestroyDnVec(vecY) )
    // CHECK_CUSPARSE( cusparseDestroy(handle) )

    // // CUSPARSE APIs
    // cusparseHandle_t     handle2 = NULL;
    // cusparseMatDescr_t matA2;
    // cusparseMatDescr_t vecX2, vecY2;
    // void*                dBuffer2    = NULL;
    // size_t               bufferSize2 = 0;
    // CHECK_CUSPARSE( cusparseCreate(&handle2) )
    // // Create sparse matrix A in CSR format
    // CHECK_CUSPARSE( cusparseCreateCoo(&matA2, A_num_rows2, A_num_cols, A_nnz2,
    //                                   dA_rows2, dA_columns2, dA_values2,
    //                                   CUSPARSE_INDEX_64I,
    //                                   CUSPARSE_INDEX_BASE_ZERO, CUDA_R_32F) )
    // // Create dense vector X
    // CHECK_CUSPARSE( cusparseCreateDnVec(&vecX2, A_num_cols, dX, CUDA_R_32F) )
    // // Create dense vector y
    // CHECK_CUSPARSE( cusparseCreateDnVec(&vecY2, A_num_rows, dY, CUDA_R_32F) )
    // // allocate an external buffer if needed
    // CHECK_CUSPARSE( cusparseSpMV_bufferSize(
    //                              handle2, CUSPARSE_OPERATION_NON_TRANSPOSE,
    //                              &alpha, matA2, vecX2, &beta, vecY2, CUDA_R_32F,
    //                              CUSPARSE_SPMV_ALG_DEFAULT, &bufferSize2) )
    // CHECK_CUDA( cudaMalloc(&dBuffer2, bufferSize2) )

    // // execute SpMV
    // CHECK_CUSPARSE( cusparseSpMV(handle2, CUSPARSE_OPERATION_NON_TRANSPOSE,
    //                              &alpha, matA2, vecX2, &beta, vecY2, CUDA_R_32F,
    //                              CUSPARSE_SPMV_ALG_DEFAULT, dBuffer2) )

    // // destroy matrix/vector descriptors
    // CHECK_CUSPARSE( cusparseDestroySpMat(matA2) )
    // CHECK_CUSPARSE( cusparseDestroyDnVec(vecX2) )
    // CHECK_CUSPARSE( cusparseDestroyDnVec(vecY2) )
    // CHECK_CUSPARSE( cusparseDestroy(handle2) )
    // //--------------------------------------------------------------------------
    // // device result check
    // CHECK_CUDA( cudaMemcpy(hY, dY, A_num_rows2 * sizeof(float),
    //                        cudaMemcpyDeviceToHost) )
    
    // printf("%lf, %lf, %lf, %lf\n", hY[0], hY[1], hY[2], hY[3]);
    // int correct = 1;
    // for (int i = 0; i < A_num_rows; i++) {
    //     if (hY[i] != hY_result[i]) { // direct floating point comparison is not
    //         correct = 0;             // reliable
    //         break;
    //     }
    // }
    // if (correct)
    //     printf("spmv_coo_example test PASSED\n");
    // else
    //     printf("spmv_coo_example test FAILED: wrong result\n");
    // //--------------------------------------------------------------------------
    // // device memory deallocation
    // CHECK_CUDA( cudaFree(dBuffer) )
    // CHECK_CUDA( cudaFree(dA_rows) )
    // CHECK_CUDA( cudaFree(dA_columns) )
    // CHECK_CUDA( cudaFree(dA_values) )
    // CHECK_CUDA( cudaFree(dX) )
    // CHECK_CUDA( cudaFree(dY) )
    return EXIT_SUCCESS;
}