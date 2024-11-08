#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
#include <cublasXt.h>
#include <math.h>
#include <time.h>        // For clock_gettime()
#include <string.h>
#include <cblas.h>       // Include OpenBLAS or another CPU BLAS library
#include <openblas_config.h>  // For controlling OpenBLAS threads


#define CHECK_CUDA(call)                                                    \
    do {                                                                    \
        cudaError_t err = (call);                                           \
        if (err != cudaSuccess) {                                           \
            fprintf(stderr, "CUDA error in file '%s' in line %i: %s.\n",    \
                    __FILE__, __LINE__, cudaGetErrorString(err));           \
            exit(EXIT_FAILURE);                                             \
        }                                                                   \
    } while (0)

#define CHECK_CUBLAS(call)                                                  \
    do {                                                                    \
        cublasStatus_t err = (call);                                        \
        if (err != CUBLAS_STATUS_SUCCESS) {                                 \
            fprintf(stderr, "cuBLAS error in file '%s' in line %i: %d.\n",  \
                    __FILE__, __LINE__, err);                               \
            exit(EXIT_FAILURE);                                             \
        }                                                                   \
    } while (0)

#define NUM_RUNS 20
#define WARMUP_RUNS 5

double get_elapsed_time(struct timespec *start, struct timespec *end) {
    double elapsed_sec = end->tv_sec - start->tv_sec;
    double elapsed_nsec = end->tv_nsec - start->tv_nsec;
    return elapsed_sec * 1e3 + elapsed_nsec / 1e6; // Return time in milliseconds
}

int compare_doubles(const void *a, const void *b) {
    double val_a = *(const double*)a;
    double val_b = *(const double*)b;
    if (val_a < val_b)
        return -1;
    else if (val_a > val_b)
        return 1;
    else
        return 0;
}

void checkMemoryLocation(void* ptr) {
    struct cudaPointerAttributes attributes;
    cudaError_t err = cudaPointerGetAttributes(&attributes, ptr);

    if (err == cudaSuccess) {
        if (attributes.type == cudaMemoryTypeDevice) {
            printf("pointer is in device memory on device %d\n", attributes.device);
        } else if (attributes.type == cudaMemoryTypeHost) {
            printf("pointer is in host memory\n");
        } else if (attributes.type == cudaMemoryTypeManaged) {
            printf("pointer is in managed memory\n");
        } else {
            printf("pointer location is unknown\n");
        }
    } else {
        printf("error: %s\n", cudaGetErrorString(err));
    }
}

// CPU SGEMM routine using OpenBLAS (or another BLAS library)
void my_sgemm_(const char *transa, const char *transb, const int *m,
               const int *n, const int *k, const float *alpha,
               const float *A, const int *lda,
               const float *B, const int *ldb,
               const float *beta,
               float *C, const int *ldc)
{
    // Map Fortran character inputs to CBLAS enums
    CBLAS_TRANSPOSE ta = (*transa == 'N' || *transa == 'n') ? CblasNoTrans : CblasTrans;
    CBLAS_TRANSPOSE tb = (*transb == 'N' || *transb == 'n') ? CblasNoTrans : CblasTrans;

    cblas_sgemm(CblasColMajor, ta, tb,
                *m, *n, *k,
                *alpha,
                A, *lda,
                B, *ldb,
                *beta,
                C, *ldc);
}

int main(int argc, char* argv[]) {
    // Default matrix dimensions
    int M;
    int N;
    int K;

    // Default number of GPUs to use
    int gpus;

    // Default CPU ratio
    double cpuRatio;
    double warmup;

    // Number of CPU threads (to be passed as CLA)
    int procs;
    
    int repeat;

    N = atoi(argv[1]);
    cpuRatio = atof(argv[2]);
    procs = atoi(argv[3]);
    gpus = atoi(argv[4]);
    warmup = atof(argv[5]);
    repeat = atoi(argv[6]);

    M = N;
    K = N;

    openblas_set_num_threads(procs);


    // Get the number of available GPUs
    int deviceCount = 0;
    CHECK_CUDA(cudaGetDeviceCount(&deviceCount));

    if (gpus > deviceCount) {
        fprintf(stderr, "Warning: Requested %d GPUs, but only %d are available. Using %d GPUs.\n",
                gpus, deviceCount, deviceCount);
        gpus = deviceCount;
    }

    // Leading dimensions
    int lda = M;
    int ldb = K;
    int ldc = M;

    // Scalar values
    float alpha = 1.0f;
    float beta = 0.0f;

    // Allocate host memory for matrices A, B, and C
    size_t sizeA = sizeof(float) * (size_t)lda * K;
    size_t sizeB = sizeof(float) * (size_t)ldb * N;
    size_t sizeC = sizeof(float) * (size_t)ldc * N;

    float* h_A = (float*)malloc(sizeA);
    float* h_B = (float*)malloc(sizeB);
    float* h_C = (float*)malloc(sizeC);

    float* d_A = NULL;
    float* d_B = NULL;
    float* d_C = NULL;


    if (h_A == NULL || h_B == NULL || h_C == NULL) {
        fprintf(stderr, "Error: Host memory allocation failed.\n");
        exit(EXIT_FAILURE);
    }

    // Initialize matrices A and B with random values, C with zeros
    for (size_t i = 0; i < (size_t)lda * K; ++i) {
        h_A[i] = 1.0;
        // rand() / (float)RAND_MAX;
    }
    for (size_t i = 0; i < (size_t)ldb * N; ++i) {
        h_B[i] = 3.0;
        // rand() / (float)RAND_MAX;
    }
    memset(h_C, 0, sizeC);

    // Create cuBLAS-XT handle
    cublasXtHandle_t handle;
    CHECK_CUBLAS(cublasXtCreate(&handle));

    // Specify the GPUs to use
    int devices[gpus];
    for (int i = 0; i < gpus; ++i) {
        devices[i] = i; // Use device IDs 0, 1, ..., gpus - 1
    }
    CHECK_CUBLAS(cublasXtDeviceSelect(handle, gpus, devices));

    // Set the CPU routine for single-precision GEMM
    CHECK_CUBLAS(cublasXtSetCpuRoutine(handle,
                                       CUBLASXT_GEMM,
                                       CUDA_R_32F,
                                       (void *)my_sgemm_));

    // Set the CPU ratio
    // Set the CPU ratio for GEMM operations (single-precision float)
    CHECK_CUBLAS(cublasXtSetCpuRatio(handle, CUBLASXT_GEMM, CUBLASXT_FLOAT, cpuRatio));

    // Arrays to hold timing data
    double times[NUM_RUNS];

    struct timespec start, end;

    // Warm-up runs
    for (int i = 0; i < WARMUP_RUNS; ++i) {
        // Free and reallocate device memory before each run
        if (d_A != NULL) CHECK_CUDA(cudaFree(d_A));
        if (d_B != NULL) CHECK_CUDA(cudaFree(d_B));
        if (d_C != NULL) CHECK_CUDA(cudaFree(d_C));

        CHECK_CUDA(cudaMalloc((void**)&d_A, sizeA));
        CHECK_CUDA(cudaMalloc((void**)&d_B, sizeB));
        CHECK_CUDA(cudaMalloc((void**)&d_C, sizeC));

        // Copy data from host to device
        CHECK_CUDA(cudaMemcpy(d_A, h_A, sizeA, cudaMemcpyHostToDevice));
        CHECK_CUDA(cudaMemcpy(d_B, h_B, sizeB, cudaMemcpyHostToDevice));

        CHECK_CUBLAS(cublasXtSgemm(handle,
                            CUBLAS_OP_N, CUBLAS_OP_N,
                            M, N, K,
                            &alpha,
                            d_A, lda,
                            d_B, ldb,
                            &beta,
                            d_C, ldc));

        // Copy result from device to host
        CHECK_CUDA(cudaMemcpy(h_C, d_C, sizeC, cudaMemcpyDeviceToHost));

        CHECK_CUDA(cudaDeviceSynchronize());
    }

    // Main timing runs
    for (int run = 0; run < NUM_RUNS; ++run) {
        if (d_A != NULL) CHECK_CUDA(cudaFree(d_A));
        if (d_B != NULL) CHECK_CUDA(cudaFree(d_B));
        if (d_C != NULL) CHECK_CUDA(cudaFree(d_C));

        clock_gettime(CLOCK_MONOTONIC, &start);

        CHECK_CUDA(cudaMalloc((void**)&d_A, sizeA));
        CHECK_CUDA(cudaMalloc((void**)&d_B, sizeB));
        CHECK_CUDA(cudaMalloc((void**)&d_C, sizeC));

        // Copy data from host to device
        CHECK_CUDA(cudaMemcpy(d_A, h_A, sizeA, cudaMemcpyHostToDevice));
        CHECK_CUDA(cudaMemcpy(d_B, h_B, sizeB, cudaMemcpyHostToDevice));

        checkMemoryLocation(d_A);
        checkMemoryLocation(d_B);

        CHECK_CUBLAS(cublasXtSgemm(handle,
                            CUBLAS_OP_N, CUBLAS_OP_N,
                            M, N, K,
                            &alpha,
                            d_A, lda,
                            d_B, ldb,
                            &beta,
                            d_C, ldc));

        CHECK_CUDA(cudaMemcpy(h_C, d_C, sizeC, cudaMemcpyDeviceToHost));

        CHECK_CUDA(cudaDeviceSynchronize());

        // End timing
        clock_gettime(CLOCK_MONOTONIC, &end);

        // Calculate elapsed time in milliseconds
        times[run] = get_elapsed_time(&start,&end)/1000.0;
        printf("time %.4fs\n", times[run]);
    }

    // Destroy cuBLAS-XT handle and free resources
    CHECK_CUBLAS(cublasXtDestroy(handle));
    free(h_A);
    free(h_B);
    free(h_C);

        // Calculate total and find minimum
    double total = 0.0;
    double min = times[0];
    for (int i = 0; i < NUM_RUNS; i++) {
        total += times[i];
        if (times[i] < min) {
            min = times[i];
        }
    }

    // Calculate average
    double avg = total / NUM_RUNS;

    // Calculate median
    // First, sort the array
    qsort(times, NUM_RUNS, sizeof(double), compare_doubles);

    double med;
    if (NUM_RUNS % 2 == 0) {
        // If even, median is the average of the two middle values
        med = (times[NUM_RUNS / 2 - 1] + times[NUM_RUNS / 2]) / 2.0;
    } else {
        // If odd, median is the middle value
        med = times[NUM_RUNS / 2];
    }

    printf("\n\n");
    // Print results
    printf("min\t %.4fs\n", min);
    printf("med\t %.4fs\n", med);
    printf("avg\t %.4fs\n", avg);
    printf("total\t %.4fs\n", total);

    return 0;
}
