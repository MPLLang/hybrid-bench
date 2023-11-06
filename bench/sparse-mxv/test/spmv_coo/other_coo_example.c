#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
#include "cusparse_v2.h"
#include <cuda_runtime_api.h>
// #include <iostream>
// #include <iomanip>
#include <assert.h>
#include <time.h>
#include <sys/time.h>

#define CUSPARSE_CHECK(x) {cusparseStatus_t _c=x; if (_c != CUSPARSE_STATUS_SUCCESS) {printf("cusparse fail: %d, line: %d\n", (int)_c, __LINE__); if(_c == CUSPARSE_STATUS_MATRIX_TYPE_NOT_SUPPORTED) {printf("CUSPARSE_STATUS_MATRIX_TYPE_NOT_SUPPORTED\n");} if(_c == CUSPARSE_STATUS_INTERNAL_ERROR) {printf("CUSPARSE_STATUS_INTERNAL_ERROR\n");} exit(-1);}}

#define CLEANUP(s)                                   \
do {                                                 \
    printf ("%s\n", s);                              \
    if (yHostPtr)           free(yHostPtr);          \
    if (zHostPtr)           free(zHostPtr);          \
    if (xIndHostPtr)        free(xIndHostPtr);       \
    if (xValHostPtr)        free(xValHostPtr);       \
    if (cooRowIndexHostPtr) free(cooRowIndexHostPtr);\
    if (cooColIndexHostPtr) free(cooColIndexHostPtr);\
    if (cooValHostPtr)      free(cooValHostPtr);     \
    if (y)                  cudaFree(y);             \
    if (z)                  cudaFree(z);             \
    if (xInd)               cudaFree(xInd);          \
    if (xVal)               cudaFree(xVal);          \
    if (csrRowPtr)          cudaFree(csrRowPtr);     \
    if (cooRowIndex)        cudaFree(cooRowIndex);   \
    if (cooColIndex)        cudaFree(cooColIndex);   \
    if (cooVal)             cudaFree(cooVal);        \
    if (descr)              cusparseDestroyMatDescr(descr);\
    if (handle)             cusparseDestroy(handle); \
    cudaDeviceReset();          \
    fflush (stdout);                                 \
} while (0)

double timerval()
    {
        struct timeval st;
        gettimeofday(&st, NULL);
        return (st.tv_sec+st.tv_usec*1e-6);
    }

int main(){     
cudaError_t cudaStat1 = cudaSuccess,cudaStat2 = cudaSuccess,cudaStat3 = cudaSuccess,cudaStat4 = cudaSuccess,cudaStat5 = cudaSuccess,cudaStat6 = cudaSuccess;
cusparseStatus_t status;
cusparseHandle_t handle=0;
cusparseMatDescr_t descr=0;
int *    cooRowIndexHostPtr=0;
int *    cooColIndexHostPtr=0;    
double * cooValHostPtr=0;
int *    cooRowIndex=0;
int *    cooColIndex=0;    
float * cooVal=0;
int *    xIndHostPtr=0;
double * xValHostPtr=0;
double * yHostPtr=0;
int *    xInd=0;
double * xVal=0;
double * y=0;  
int *    csrRowPtr=0;
int *    csrColPtr = 0; 
double * zHostPtr=0; 
double * z=0; 
int      n, nnz, nnz_vector;
double dzero =0.0;
double dtwo  =2.0;
double dthree=3.0;
double dfive =5.0;
cusparseStatus_t stat;
double avg_time = 0, s_time, e_time;
cusparseMatDescr_t descrA, descrB, descrC;


stat = cusparseCreateMatDescr(&descrA);
CUSPARSE_CHECK(stat);

stat = cusparseCreateMatDescr(&descrB);
CUSPARSE_CHECK(stat);

stat = cusparseCreateMatDescr(&descrC);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatType(descrA, CUSPARSE_MATRIX_TYPE_GENERAL);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatType(descrB, CUSPARSE_MATRIX_TYPE_GENERAL);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatType(descrC, CUSPARSE_MATRIX_TYPE_GENERAL);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatIndexBase(descrA, CUSPARSE_INDEX_BASE_ZERO);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatIndexBase(descrB, CUSPARSE_INDEX_BASE_ZERO);
CUSPARSE_CHECK(stat);

stat = cusparseSetMatIndexBase(descrC, CUSPARSE_INDEX_BASE_ZERO);
CUSPARSE_CHECK(stat);


printf("testing example\n");
/* create the following sparse test matrix in COO format */
/* |1.0     2.0 3.0|
   |    4.0        |
   |5.0     6.0 7.0|
   |    8.0     9.0| */
n=4; nnz=9; 
cooRowIndexHostPtr = (int *)   malloc(nnz*sizeof(cooRowIndexHostPtr[0])); 
cooColIndexHostPtr = (int *)   malloc(nnz*sizeof(cooColIndexHostPtr[0])); 
cooValHostPtr      = (double *)malloc(nnz*sizeof(cooValHostPtr[0])); 
if ((!cooRowIndexHostPtr) || (!cooColIndexHostPtr) || (!cooValHostPtr)){
    CLEANUP("Host malloc failed (matrix)");
    return 1; 
}
cooRowIndexHostPtr[0]=0; cooColIndexHostPtr[0]=0; cooValHostPtr[0]=1.0;  
cooRowIndexHostPtr[1]=0; cooColIndexHostPtr[1]=2; cooValHostPtr[1]=2.0;  
cooRowIndexHostPtr[2]=0; cooColIndexHostPtr[2]=3; cooValHostPtr[2]=3.0;  
cooRowIndexHostPtr[3]=1; cooColIndexHostPtr[3]=1; cooValHostPtr[3]=4.0;  
cooRowIndexHostPtr[4]=2; cooColIndexHostPtr[4]=0; cooValHostPtr[4]=5.0;  
cooRowIndexHostPtr[5]=2; cooColIndexHostPtr[5]=2; cooValHostPtr[5]=6.0;
cooRowIndexHostPtr[6]=2; cooColIndexHostPtr[6]=3; cooValHostPtr[6]=7.0;  
cooRowIndexHostPtr[7]=3; cooColIndexHostPtr[7]=1; cooValHostPtr[7]=8.0;  
cooRowIndexHostPtr[8]=3; cooColIndexHostPtr[8]=3; cooValHostPtr[8]=9.0;  

//print the matrix
printf("Input data:\n");
for (int i=0; i<nnz; i++){        
    printf("cooRowIndexHostPtr[%d]=%d  ",i,cooRowIndexHostPtr[i]);
    printf("cooColIndexHostPtr[%d]=%d  ",i,cooColIndexHostPtr[i]);
    printf("cooValHostPtr[%d]=%f     \n",i,cooValHostPtr[i]);
}

/* allocate GPU memory and copy the matrix and vectors into it */
cudaStat1 = cudaMalloc((void**)&cooRowIndex,nnz*sizeof(cooRowIndex[0])); 
cudaStat2 = cudaMalloc((void**)&cooColIndex,nnz*sizeof(cooColIndex[0]));
cudaStat3 = cudaMalloc((void**)&cooVal,     nnz*sizeof(cooVal[0])); 

if ((cudaStat1 != cudaSuccess) ||
    (cudaStat2 != cudaSuccess) ||
    (cudaStat3 != cudaSuccess) ||
    (cudaStat4 != cudaSuccess) ||
    (cudaStat5 != cudaSuccess) ||
    (cudaStat6 != cudaSuccess)) {
    CLEANUP("Device malloc failed");
    return 1; 
}    
cudaStat1 = cudaMemcpy(cooRowIndex, cooRowIndexHostPtr, 
                       (size_t)(nnz*sizeof(cooRowIndex[0])), 
                       cudaMemcpyHostToDevice);
cudaStat2 = cudaMemcpy(cooColIndex, cooColIndexHostPtr, 
                       (size_t)(nnz*sizeof(cooColIndex[0])), 
                       cudaMemcpyHostToDevice);
cudaStat3 = cudaMemcpy(cooVal,      cooValHostPtr,      
                       (size_t)(nnz*sizeof(cooVal[0])),      
                       cudaMemcpyHostToDevice);

if ((cudaStat1 != cudaSuccess) ||
    (cudaStat2 != cudaSuccess) ||
    (cudaStat3 != cudaSuccess) ||
    (cudaStat4 != cudaSuccess) ||
    (cudaStat5 != cudaSuccess) ||
    (cudaStat6 != cudaSuccess)) {
    CLEANUP("Memcpy from Host to Device failed");
    return 1;
}

/* initialize cusparse library */
status= cusparseCreate(&handle);
if (status != CUSPARSE_STATUS_SUCCESS) {
    CLEANUP("CUSPARSE Library initialization failed");
    return 1;
}

/* create and setup matrix descriptor */ 
status= cusparseCreateMatDescr(&descr); 
if (status != CUSPARSE_STATUS_SUCCESS) {
    CLEANUP("Matrix descriptor initialization failed");
    return 1;
}   

cusparseSetMatType(descr,CUSPARSE_MATRIX_TYPE_GENERAL);
cusparseSetMatIndexBase(descr,CUSPARSE_INDEX_BASE_ZERO);  

/* exercise conversion routines (convert matrix from COO 2 CSR format) */
cudaStat1 = cudaMalloc(&csrRowPtr,(n+1)*sizeof(csrRowPtr[0]));
if (cudaStat1 != cudaSuccess) {
    CLEANUP("Device malloc failed (csrRowPtr)");
    return 1;
}

status= cusparseXcoo2csr(handle,cooRowIndex,nnz,n,
                         csrRowPtr,CUSPARSE_INDEX_BASE_ZERO); 
if (status != CUSPARSE_STATUS_SUCCESS) {
    CLEANUP("Conversion from COO to CSR format failed");
    return 1;
} 

/*
int *csr_values;
csr_values = (int *)malloc((n+1)*sizeof(int)); 
cudaStat3 = cudaMemcpy(csr_values, csrRowPtr, (n+1)*sizeof(int), cudaMemcpyDeviceToHost);
    if (cudaStat3 != cudaSuccess) {
    CLEANUP("Device memcopy failed: csr values");
    return 1;
    }
printf("CSR values are \n");    
for(int y2 =0; y2< n+1; y2++)
    printf("%d \t", csr_values[y2]);    

*/
/*

int y1;
    printf("\n");
    printf("col orig  is\n");
    for(y1 =0; y1 < nnz; y1++)
    {
        printf("%d\t", cooColIndex[y1]);
    }
        printf("\n");
    printf("nnz orig is\n");
    for(y1 =0; y1 < nnz; y1++)
    {
        printf("%f\t", h_csrValC[y1]);
    }

*/


//csrRowPtr data is present now
//csrRowPtr, cooColIndex, cooVal (all the three are matrix A data) shall be used from here for the operation

int nnzA = nnz, nnzB = nnz, nnzC;
cusparseOperation_t transA = CUSPARSE_OPERATION_NON_TRANSPOSE;
cusparseOperation_t transB = CUSPARSE_OPERATION_NON_TRANSPOSE;

// figure out size of C
int baseC;
int *csrRowPtrC, *csrColIndC;
float *csrValC;

// nnzTotalDevHostPtr points to host memory
int *nnzTotalDevHostPtr = &nnzC;
    stat = cusparseSetPointerMode(handle, CUSPARSE_POINTER_MODE_HOST);
    CUSPARSE_CHECK(stat);

cudaStat1 = cudaMalloc((void**)&csrRowPtrC, sizeof(int)*(n+1));
    if (cudaStat1 != cudaSuccess) {
    CLEANUP("Device malloc failed (csrRowPtr)");
    return 1;
}

s_time=timerval();
//from here add code to multiply

/*

*/  

stat = cusparseXcsrgemmNnz(handle, transA, transB, n, n, n,
                                descrA, nnzA, csrRowPtr, cooColIndex /*csrColInd*/,
                                descrB, nnzB, csrRowPtr, cooColIndex /*csrColInd*/,
                                descrC, csrRowPtrC, nnzTotalDevHostPtr );
    CUSPARSE_CHECK(stat);

    if (NULL != nnzTotalDevHostPtr)
    {
        nnzC = *nnzTotalDevHostPtr;
    }
    else{
    cudaStat1 = cudaMemcpy(&nnzC, csrRowPtrC+n, sizeof(int), cudaMemcpyDeviceToHost);
    cudaStat2 = cudaMemcpy(&baseC, csrRowPtrC, sizeof(int), cudaMemcpyDeviceToHost);
    if (cudaStat1 || cudaStat2 != cudaSuccess) {
    CLEANUP("Device malloc failed (csrRowPtr)");
    return 1;
    }
        nnzC -= baseC;}

    cudaStat1 = cudaMalloc((void**)&csrColIndC, sizeof(int)*nnzC);
    if (cudaStat1 != cudaSuccess) {
    CLEANUP("Device malloc failed (csrColIndC)");
    return 1;
    }

    cudaStat1 = cudaMalloc((void**)&csrValC, sizeof(float)*nnzC);
    if (cudaStat1 != cudaSuccess) {
    CLEANUP("Device malloc failed (csrValC)");
    return 1;
    }

    stat = cusparseScsrgemm(handle, transA, transB, n, n, n,
    descrA, nnzA,
    cooVal/*csrValA*/, csrRowPtr, cooColIndex,
    descrB, nnzB,
    cooVal/*csrValA*/, csrRowPtr, cooColIndex,
    descrC,
    csrValC/*csrValA*/, csrRowPtrC, csrColIndC);

    CUSPARSE_CHECK(stat);

    cudaDeviceSynchronize();

    int *h_csrRowPtrC = NULL, *h_csrColIndC = NULL; 
    float *h_csrValC = NULL;
    h_csrValC =  (float *)malloc(nnzC*sizeof(float));
    h_csrRowPtrC = (int *)malloc(n+1*sizeof(int));
    h_csrColIndC = (int *)malloc(nnzC*sizeof(int)); 

    cudaStat1 = cudaMemcpy(h_csrRowPtrC, csrRowPtrC, (n+1)*sizeof(int), cudaMemcpyDeviceToHost);
    if (cudaStat1 != cudaSuccess) {
    CLEANUP("Device memcopy failed csrRowPtrC");
    return 1;
    }

    cudaStat2 = cudaMemcpy(h_csrColIndC, csrColIndC,  nnzC*sizeof(int), cudaMemcpyDeviceToHost);
    if (cudaStat2 != cudaSuccess) {
    CLEANUP("Device memcopy failed: cooColIndex");
    return 1;
    }

    printf("nnz value is %d, nnzc is %d\n", nnz, nnzC);


    cudaStat3 = cudaMemcpy(h_csrValC, csrValC, nnzC*sizeof(float), cudaMemcpyDeviceToHost);
    if (cudaStat3 != cudaSuccess) {
    CLEANUP("Device memcopy failed: csrValC");
    return 1;
    }

    int y1;
    printf("row is\n");
    for(y1 =0; y1 < n+1; y1++)
    {
        printf("%d\t", h_csrRowPtrC[y1]);
    }
    printf("\n");
    printf("col is\n");
    for(y1 =0; y1 < nnzC; y1++)
    {
        printf("%d\t", h_csrColIndC[y1]);
    }
        printf("\n");
    printf("nnz is\n");
    for(y1 =0; y1 < nnzC; y1++)
    {
        printf("%f\t", h_csrValC[y1]);
    }

    /* destroy matrix descriptor */ 
    status = cusparseDestroyMatDescr(descr); 
    descr = 0;
    if (status != CUSPARSE_STATUS_SUCCESS) {
        CLEANUP("Matrix descriptor destruction failed");
        return 1;
    }    

    /* destroy handle */
    status = cusparseDestroy(handle);
    handle = 0;
    if (status != CUSPARSE_STATUS_SUCCESS) {
        CLEANUP("CUSPARSE Library release of resources failed");
        return 1;
    }   

    cudaFree(csrRowPtr);
    cudaFree(cooColIndex);
    cudaFree(cooRowIndex);
    cudaFree(cooVal);
    cudaFree(csrRowPtrC);
    cudaFree(csrColIndC);
    cudaFree(csrValC);

    return 0;
}