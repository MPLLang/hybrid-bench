include ../config.mk

main.mpl.bin: phony futmandelbrot
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futmandelbrot/mandelbrot.c futmandelbrot/mandelbrot.smlfut.c

main.mlton.bin: phony futmandelbrot
	$(MLTON) $(MLTONFLAGS) -output main.mlton.bin main.mlton.mlb futmandelbrot/mandelbrot.c futmandelbrot/mandelbrot.smlfut.c

# main.mpl.dbg: phony futmandelbrot
# 	$(MPL) $(MLTONFLAGS) -debug true -debug-runtime true -keep g -output main.mpl.dbg main.mlb ../common/timer.c futmandelbrot/mandelbrot.c

futmandelbrot: phony
	$(MAKE) -C futmandelbrot

clean: 
	rm -f main.mpl
	$(MAKE) -C futmandelbrot clean

.PHONY: phony
phony:
