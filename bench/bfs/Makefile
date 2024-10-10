include ../config.mk

SYSMPL=mpl

main.mpl.bin: phony futhark/bfs.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/bfs.c futhark/bfs.smlfut.c

main.cpu-only.sysmpl.bin: phony
	$(SYSMPL) $(MLTONFLAGS) -output main.cpu-only.sysmpl.bin main.cpu-only.mlb

futhark/bfs.sml: futhark/bfs.fut futhark/lib
	futhark $(FUTHARK_BACKEND) --library futhark/bfs.fut
	smlfut -o futhark --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/bfs.json

futhark/lib:
	(cd futhark && futhark pkg sync)

clean:
	rm -f futhark/bfs.json futhark/bfs.c futhark/bfs.h  futhark/bfs.sml futhark/bfs.sig \
	  bfs.smlfut.c main.mpl.bin

.PHONY: phony
phony:
