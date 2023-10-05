include ../config.mk

test.mpl: phony bigadd.smlfut.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c bigadd.c bigadd.smlfut.c

bigadd.smlfut.c: bigadd.c
	smlfut -o . --poly-arrays --structure=Futhark --signature=FUTHARK bigadd.json

bigadd.c: bigadd.fut
	futhark $(FUTHARK_BACKEND) --library bigadd.fut
clean:
	rm -f test.mpl bigadd.c

.PHONY: phony
phony:
