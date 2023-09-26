MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-disable-pass splitTypes1 \
	-disable-pass splitTypes2 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/ -D_GNU_SOURCE' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=/home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl

main.mpl.bin: phony futray
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c futray/ray.c

main.mpl: phony futray
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futray/ray.c

futray: phony
	$(MAKE) -C futray

clean: 
	rm main.mpl
	$(MAKE) -C futray clean

.PHONY: phony
phony:
