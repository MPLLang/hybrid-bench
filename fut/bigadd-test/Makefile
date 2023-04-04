MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=/home/ec2-user/mpl-hybrid-sched/build/bin/mpl

test.mpl: phony bigadd.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c bigadd.c glue.c

bigadd.c: phony
	~/futhark-0.23.1-linux-x86_64/bin/futhark cuda --library bigadd.fut

clean: 
	rm test.mpl

.PHONY: phony
phony:
