include ../config.mk

main.mpl.bin: phony sieve.c
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin test.mlb ../common/timer.c sieve.c glue.c

test.mpl: phony sieve.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb ../common/timer.c sieve.c glue.c

test.mpl.dbg: phony sieve.c
	$(MPL) $(MLTONFLAGS) -debug true -keep g -output test.mpl.dbg test.mlb ../common/timer.c sieve.c glue.c

sieve.c: phony
	futhark $(FUTHARK_BACKEND) --library sieve.fut

clean: 
	rm test.mpl

.PHONY: phony
phony:
