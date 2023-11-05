include ../config.mk

main.mpl.bin: phony futhark/kmeans.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/kmeans.c futhark/kmeans.smlfut.c

futhark/kmeans.sml: futhark/kmeans.fut
	futhark $(FUTHARK_BACKEND) --library futhark/kmeans.fut
	smlfut -o futhark --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/kmeans.json

clean:
	rm -f futhark/kmeans.json futhark/kmeans.c futhark/kmeans.h  futhark/kmeans.sml futhark/kmeans.sig \
	  kmeans.smlfut.c main.mpl.bin

.PHONY: phony
phony:
