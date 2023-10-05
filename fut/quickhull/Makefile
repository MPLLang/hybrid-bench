include ../config.mk

main.mpl.bin: phony futhark/quickhull.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/quickhull.c futhark/quickhull.smlfut.c

futhark/quickhull.sml: futhark/quickhull.fut
	futhark $(FUTHARK_BACKEND) --library futhark/quickhull.fut
	smlfut -o futhark --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/quickhull.json

clean:
	rm -f futhark/quickhull.json futhark/quickhull.c futhark/quickhull.h  futhark/quickhull.sml futhark/quickhull.sig \
	  quickhull.smlfut.c main.mpl.bin

.PHONY: phony
phony:
