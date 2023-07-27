MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/ -I/home/ec2-user/openblas-v0.3.23/installed/include/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -L/home/ec2-user/openblas-v0.3.23/installed/lib/ -lopenblas -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

# MPL=/home/ec2-user/mpl-hybrid-sched/build/bin/mpl
MPL=/home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl


test.mpl: phony blas_library.o
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c blas_library.o glue_cpu_blas.c

blas_library.o: glue.cu
	nvcc -I../common/ -lcublas -c -o blas_library.o glue.cu -O3 -llapack -lblas -lpthread

clean: 
	rm test.mpl

.PHONY: phony
phony:
