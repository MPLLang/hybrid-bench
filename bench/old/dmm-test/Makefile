include ../config.mk

test.mpl: phony blas_library.o
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c blas_library.o glue_cpu_blas.c

blas_library.o: glue.cu
	nvcc -I../common/ -lcublas -c -o blas_library.o glue.cu -O3 -llapack -lblas -lpthread
clean: 
	rm test.mpl

.PHONY: phony
phony:
