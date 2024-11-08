NVCC = nvcc
CUBLAS_LIB = -lcublas

OPENBLAS_FLAGS = \
	-I/home/cc/openblas-0.3.28-cublasxt/build/include/ \
	-L/home/cc/openblas-0.3.28-cublasxt/build/lib/ -lopenblas


all: dmm-cpu dmm-hybrid

dmm-cpu: dmm-cpu.cpp
	$(NVCC) -o dmm-cpu dmm-cpu.cpp $(CUBLAS_LIB) $(OPENBLAS_FLAGS)

dmm-hybrid: dmm-hybrid.cpp
	$(NVCC) -o dmm-hybrid dmm-hybrid.cpp $(CUBLAS_LIB) $(OPENBLAS_FLAGS)

clean:
	rm -f dmm-cpu dmm-hybrid

