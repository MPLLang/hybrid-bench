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
    int M = 4096;
    int N = 4096;
    int K = 4096;

    // Default number of GPUs to use
    int numGPUs = 1;

    // Default CPU ratio
    double cpuRatio = 0.1;

    // Number of CPU threads (to be passed as CLA)
    int cpuThreads = 1;

    // Parse command-line arguments
    if (argc != 7) {
        fprintf(stderr, "Usage: %s <num_gpus> <M> <N> <K> <cpu_ratio> <cpu_threads>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    numGPUs = atoi(argv[1]);
    M = atoi(argv[2]);
    N = atoi(argv[3]);
    K = atoi(argv[4]);
    cpuRatio = atof(argv[5]);
    cpuThreads = atoi(argv[6]);


    if (numGPUs < 1 || M < 1 || N < 1 || K < 1 || cpuRatio < 0.0 || cpuRatio > 1.0 || cpuThreads < 1) {
        fprintf(stderr, "Error: Invalid input values.\n");
        fprintf(stderr, "num_gpus, M, N, K must be positive integers.\n");
        fprintf(stderr, "cpu_ratio must be a floating-point value between 0.0 and 1.0.\n");
        fprintf(stderr, "cpu_threads must be a positive integer.\n");

        exit(EXIT_FAILURE);
    }

    openblas_set_num_threads(cpuThreads);


    // Get the number of available GPUs
    int deviceCount = 0;
    CHECK_CUDA(cudaGetDeviceCount(&deviceCount));

    if (numGPUs > deviceCount) {
        fprintf(stderr, "Warning: Requested %d GPUs, but only %d are available. Using %d GPUs.\n",
                numGPUs, deviceCount, deviceCount);
        numGPUs = deviceCount;
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
    int devices[numGPUs];
    for (int i = 0; i < numGPUs; ++i) {
        devices[i] = i; // Use device IDs 0, 1, ..., numGPUs - 1
    }
    CHECK_CUBLAS(cublasXtDeviceSelect(handle, numGPUs, devices));

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
        CHECK_CUBLAS(cublasXtSgemm(handle,
                                   CUBLAS_OP_N, CUBLAS_OP_N,
                                   M, N, K,
                                   &alpha,
                                   h_A, lda,
                                   h_B, ldb,
                                   &beta,
                                   h_C, ldc));
        // Synchronize to ensure the operation is complete
        CHECK_CUDA(cudaDeviceSynchronize());
    }

    // Main timing runs
    for (int run = 0; run < NUM_RUNS; ++run) {
        // Reset matrix C to zero before each run
        memset(h_C, 0, sizeC);

        // Start timing
        clock_gettime(CLOCK_MONOTONIC, &start);

        // Perform matrix multiplication
        CHECK_CUBLAS(cublasXtSgemm(handle,
                                   CUBLAS_OP_N, CUBLAS_OP_N,
                                   M, N, K,
                                   &alpha,
                                   h_A, lda,
                                   h_B, ldb,
                                   &beta,
                                   h_C, ldc));

        // Synchronize to ensure the operation is complete
        CHECK_CUDA(cudaDeviceSynchronize());

        // End timing
        clock_gettime(CLOCK_MONOTONIC, &end);

        // Calculate elapsed time in milliseconds
        times[run] = get_elapsed_time(&start, &end);
    }

    // Destroy cuBLAS-XT handle and free resources
    CHECK_CUBLAS(cublasXtDestroy(handle));
    free(h_A);
    free(h_B);
    free(h_C);

    // Calculate statistics
    double sum = 0.0;
    double min_time = times[0];
    double max_time = times[0];
    for (int i = 0; i < NUM_RUNS; ++i) {
        sum += times[i];
        if (times[i] < min_time) min_time = times[i];
        if (times[i] > max_time) max_time = times[i];
    }
    double average = sum / NUM_RUNS;

    // Calculate standard deviation
    double variance = 0.0;
    for (int i = 0; i < NUM_RUNS; ++i) {
        variance += (times[i] - average) * (times[i] - average);
    }
    variance /= NUM_RUNS;
    double stddev = sqrt(variance);

    // Calculate median
    qsort(times, NUM_RUNS, sizeof(double), compare_doubles);
    double median;
    if (NUM_RUNS % 2 == 0) {
        // Even number of runs
        median = (times[NUM_RUNS / 2 - 1] + times[NUM_RUNS / 2]) / 2.0;
    } else {
        // Odd number of runs
        median = times[NUM_RUNS / 2];
    }

    // Print raw timing results
    printf("Raw Timing Results (in milliseconds):\n");
    for (int i = 0; i < NUM_RUNS; ++i) {
        printf("Run %d: %.3f ms\n", i + 1, times[i]);
    }

    // Print statistics
    printf("\nHybrid Benchmark Results over %d runs:\n", NUM_RUNS);
    printf("Matrix Dimensions: M=%d, N=%d, K=%d\n", M, N, K);
    printf("Number of GPUs Used: %d\n", numGPUs);
    printf("CPU Ratio: %.2f\n", cpuRatio);
    printf("Average Time: %.3f ms\n", average);
    printf("Minimum Time: %.3f ms\n", min_time);
    printf("Median Time: %.3f ms\n", median);
    printf("Maximum Time: %.3f ms\n", max_time);
    printf("Standard Deviation: %.3f ms\n", stddev);

    return 0;
}
