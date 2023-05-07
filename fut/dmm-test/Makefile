MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

# MPL=/home/ec2-user/mpl-hybrid-sched/build/bin/mpl
MPL=~/mpl/build/bin/./mpl

test.mpl: phony blas_library.o
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c 

blas_library: blas_library.cu
	nvcc -lcublas -c -o blas_library.o blas_library.cu -O3 -llapack -lblas

clean: 
	rm test.mpl

.PHONY: phony
phony:
