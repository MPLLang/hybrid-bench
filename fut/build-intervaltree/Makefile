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

main.mpl: phony futsort
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futsort/sort.c glue.c

futsort: phony
	$(MAKE) -C futsort

clean: 
	rm main.mpl
	$(MAKE) -C futsort clean

.PHONY: phony
phony:
