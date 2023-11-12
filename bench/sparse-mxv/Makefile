include ../config.mk

main.mpl.bin: phony futhark/sparse-mxv.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/sparse-mxv.c futhark/sparse-mxv.smlfut.c

futhark/sparse-mxv.sml: futhark/*.fut
	futhark $(FUTHARK_BACKEND) --library futhark/sparse-mxv.fut
	smlfut -o futhark --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/sparse-mxv.json

clean:
	rm -f main.mpl.bin

.PHONY: phony
phony:
