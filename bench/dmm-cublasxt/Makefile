NVCC = nvcc
CUBLAS_LIB = -lcublas

OPENBLAS_FLAGS = \
	-I/home/cc/openblas-0.3.28-cublasxt/build/include/ \
	-L/home/cc/openblas-0.3.28-cublasxt/build/lib/ -lopenblas


all: mgpu mgpu-hybrid

mgpu: mgpu.c
	$(NVCC) -o mgpu mgpu.c $(CUBLAS_LIB)

mgpu-hybrid: mgpu-hybrid.c
	$(NVCC) -o mgpu-hybrid mgpu-hybrid.c $(CUBLAS_LIB) $(OPENBLAS_FLAGS)

clean:
	rm -f mgpu mgpu-hybrid

