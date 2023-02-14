MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=/home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl

test.mpl: phony sieve.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c sieve.c glue.c

sieve.c: phony
	futhark cuda --library sieve.fut

clean: 
	rm test.mpl

.PHONY: phony
phony:
