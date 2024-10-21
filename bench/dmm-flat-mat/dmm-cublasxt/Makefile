NVCC = nvcc
CUBLAS_LIB = -lcublas
OPENBLAS_LIB = -lopenblas

all: mgpu mgpu-hybrid

mgpu: mgpu.c
	$(NVCC) -o mgpu mgpu.c $(CUBLAS_LIB)

mgpu-hybrid: mgpu-hybrid.c
	$(NVCC) -o mgpu-hybrid mgpu-hybrid.c $(CUBLAS_LIB) $(OPENBLAS_LIB)

clean:
	rm -f mgpu mgpu-hybrid

