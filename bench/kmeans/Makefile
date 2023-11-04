include ../config.mk

main.mpl.bin: phony
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb

clean:
	rm -f futhark/quickhull.json futhark/quickhull.c futhark/quickhull.h  futhark/quickhull.sml futhark/quickhull.sig \
	  quickhull.smlfut.c main.mpl.bin

.PHONY: phony
phony:
