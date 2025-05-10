#include <cuda.h>
#include <stdio.h>
#include <malloc.h>

__global__ void gpu_thing(float* f) {
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  f[index] = index;
  //printf("+%d\n", index);
}

int main() {
  cudaEvent_t start, mid1, mid2, end;
  cudaEventCreate(&start);
  cudaEventCreate(&end);
  cudaEventCreate(&mid1);
  cudaEventCreate(&mid2);

  int size = 10000 * 10000 * 2;
  float* d;
  for (int i = 0; i < 100; i++) {
    float* f = (float*) malloc(sizeof(float) * size);
    float* f1 = (float*) malloc(sizeof(float) * size);
    cudaHostRegister(f, size * sizeof(float), 0);
    cudaHostRegister(f1, size * sizeof(float), 0);
    for (int i = 0; i < size; i++) {
      f[i] = rand();
    }
    cudaMalloc(&d, size * sizeof(float));
    cudaEventRecord(start, 0);
    cudaMemcpyAsync(d, f, size * sizeof(float), cudaMemcpyHostToDevice, 0);
    cudaEventRecord(mid1, 0);
    gpu_thing<<<size / 1000, 1000>>>(d);
    cudaEventRecord(mid2, 0);
    cudaMemcpyAsync(f1, d, size * sizeof(float), cudaMemcpyDeviceToHost, 0); 
    cudaEventRecord(end, 0);

    cudaStreamSynchronize(0);
    float time1, time2, time3;
    cudaEventElapsedTime(&time1, start, mid1);
    cudaEventElapsedTime(&time2, mid1, mid2);
    cudaEventElapsedTime(&time3, mid2, end);
    printf("%f\t%f\t%f\n", time1, time2, time3);
    cudaFree(d);

    free(f);
    free(f1);
  }
}
