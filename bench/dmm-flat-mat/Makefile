include ../config.mk

main.mpl.bin: phony blas_library.o
	$(MPL) $(MLTONFLAGS) $(OPENBLAS_MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c blas_library.o glue_cpu_blas.c

blas_library.o: glue.cu
	nvcc -I../common/ -lcublas -c -o blas_library.o glue.cu -O3 -llapack -lblas -lpthread

clean: 
	rm main.mpl.bin

.PHONY: phony
phony:
