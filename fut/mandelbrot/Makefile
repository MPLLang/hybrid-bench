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

main.mpl.bin: phony futmandelbrot
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c futmandelbrot/mandelbrot.c glue.c

main.mpl: phony futmandelbrot
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futmandelbrot/mandelbrot.c glue.c

futmandelbrot: phony
	$(MAKE) -C futmandelbrot

clean: 
	rm main.mpl
	$(MAKE) -C futmandelbrot clean

.PHONY: phony
phony:
