MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-disable-pass splitTypes1 \
	-disable-pass splitTypes2 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=/home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl

main.mpl: phony futtinykaboom
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futtinykaboom/tinykaboom.c glue.c

futtinykaboom: phony
	$(MAKE) -C futtinykaboom

clean: 
	rm main.mpl
	$(MAKE) -C futtinykaboom clean

.PHONY: phony
phony:
