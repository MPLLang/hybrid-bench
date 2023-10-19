include ../config.mk

main.mpl.bin: phony futsort/sort.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futsort/sort.c futsort/sort.smlfut.c

futsort/sort.sml: futsort/sort.fut futsort/lib
	futhark $(FUTHARK_BACKEND) --library futsort/sort.fut
	smlfut -o futsort --poly-arrays  --structure=FutharkSort --signature=FUTHARK_SORT futsort/sort.json

futsort/lib:
	(cd futsort; futhark pkg sync)

clean:
	rm -f futsort/sort.json futsort/sort.c futsort/sort.h futsort/sort.sml futsort/sort.sig \
	  futsort/sort.smlfut.c futhark.cache main.mpl.bin
	rm -rf futsort/lib

.PHONY: phony
phony:
