#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <cuda_runtime.h>
#include <cublasXt.h>
#include <math.h>
#include <time.h>        // For clock_gettime()
#include <string.h>
#include <cblas.h>       // Include OpenBLAS or another CPU BLAS library
#include <openblas_config.h>  // For controlling OpenBLAS threads




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
    procs = atoi(argv[2]);
    warmup = atof(argv[3]);
    repeat = atoi(argv[4]);

    M = N;
    K = N;

    openblas_set_num_threads(procs);

    std::cout << "Num Procs: " << openblas_get_num_procs() << std::endl;


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

    // We initialize h_A and h_B with random values to avoid data reuse
    for (size_t i = 0; i < (size_t)lda * K; ++i) {
        h_A[i] = rand() / (float)RAND_MAX;
    }
    for (size_t i = 0; i < (size_t)ldb * N; ++i) {
        h_B[i] = rand() / (float)RAND_MAX;
    }
    memset(h_C, 0, sizeC);

    // Arrays to hold timing data
    double times[repeat];

    struct timespec start, end;

    // Warm-up runs
    for (int i = 0; i < warmup; ++i) {
        // We initialize h_A and h_B with random values to avoid data reuse
        for (size_t i = 0; i < (size_t)lda * K; ++i) {
            h_A[i] = rand() / (float)RAND_MAX;
        }
        for (size_t i = 0; i < (size_t)ldb * N; ++i) {
            h_B[i] = rand() / (float)RAND_MAX;
        }
        memset(h_C, 0, sizeC);
        cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    M, N, K,
                    alpha,
                    h_A, lda,
                    h_B, ldb,
                    beta,
                    h_C, ldc);
    }

    // Main timing runs
    for (int run = 0; run < repeat; ++run) {
        // We initialize h_A and h_B with random values to avoid data reuse
        for (size_t i = 0; i < (size_t)lda * K; ++i) {
            h_A[i] = rand() / (float)RAND_MAX;
        }
        for (size_t i = 0; i < (size_t)ldb * N; ++i) {
            h_B[i] = rand() / (float)RAND_MAX;
        }
        memset(h_C, 0, sizeC);

        // Start timing
        clock_gettime(CLOCK_MONOTONIC, &start);

        cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    M, N, K,
                    alpha,
                    h_A, lda,
                    h_B, ldb,
                    beta,
                    h_C, ldc);

        clock_gettime(CLOCK_MONOTONIC, &end);
        // Calculate elapsed time in milliseconds
        times[run] = get_elapsed_time(&start,&end)/1000.0;
        printf("time %.4fs\n", times[run]);
    }

    // Destroy cuBLAS-XT handle and free resources
    free(h_A);
    free(h_B);
    free(h_C);

        // Calculate total and find minimum
    double total = 0.0;
    double min = times[0];
    for (int i = 0; i < repeat; i++) {
        total += times[i];
        if (times[i] < min) {
            min = times[i];
        }
    }

    // Calculate average
    double avg = total / repeat;

    // Calculate median
    // First, sort the array
    qsort(times, repeat, sizeof(double), compare_doubles);

    double med;
    if (repeat % 2 == 0) {
        // If even, median is the average of the two middle values
        med = (times[repeat / 2 - 1] + times[repeat / 2]) / 2.0;
    } else {
        // If odd, median is the middle value
        med = times[repeat / 2];
    }

    printf("\n\n");
    // Print results
    printf("min\t %.4fs\n", min);
    printf("med\t %.4fs\n", med);
    printf("avg\t %.4fs\n", avg);
    printf("total\t %.4fs\n", total);

    return 0;
}
