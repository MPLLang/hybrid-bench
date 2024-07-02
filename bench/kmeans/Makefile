include ../config.mk

main.mpl.bin: phony futhark/kmeans.sml ispc/squared_distance.o
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark/kmeans.c futhark/kmeans.smlfut.c ispc/squared_distance.o ispc/squared_distance_wrapper.c

main.mlton.bin: phony futhark/kmeans.sml
	$(MLTON) $(MLTONFLAGS) -output main.mlton.bin main.mlton.mlb futhark/kmeans.c futhark/kmeans.smlfut.c

ispc/squared_distance.o:
	ispc ispc/squared_distance.ispc -o ispc/squared_distance.o
	ispc ispc/squared_distance.ispc -h ispc/squared_distance.h

futhark/kmeans.sml: futhark/kmeans.fut
	futhark $(FUTHARK_BACKEND) --library futhark/kmeans.fut
	smlfut -o futhark --poly-arrays  --structure=Futhark --signature=FUTHARK futhark/kmeans.json

clean:
	rm -f futhark/kmeans.json futhark/kmeans.c futhark/kmeans.h  futhark/kmeans.sml futhark/kmeans.sig \
	  kmeans.smlfut.c main.mpl.bin

.PHONY: phony
phony:
