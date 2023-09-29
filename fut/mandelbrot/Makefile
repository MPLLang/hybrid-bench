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

main.mpl.bin: phony futmandelbrot
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c futmandelbrot/mandelbrot.c futmandelbrot/mandelbrot.smlfut.c

# main.mpl.dbg: phony futmandelbrot
# 	$(MPL) $(MLTONFLAGS) -debug true -debug-runtime true -keep g -output main.mpl.dbg main.mlb ../common/timer.c futmandelbrot/mandelbrot.c

futmandelbrot: phony
	$(MAKE) -C futmandelbrot

clean: 
	rm main.mpl
	$(MAKE) -C futmandelbrot clean

.PHONY: phony
phony:
