include ../config.mk

main.mpl.bin: phony quickhull.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/quickhull.c quickhull.smlfut.c

quickhull.sml: futhark/quickhull.fut
	futhark $(FUTHARK_BACKEND) --library futhark/quickhull.fut
	smlfut -o . --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/quickhull.json

clean:
	rm -f futhark/quickhull.json futhark/quickhull.c futhark/quickhull.h quickhull.smlfut.c main.mpl.bin quickhull.sml quickhull.sig

.PHONY: phony
phony:
