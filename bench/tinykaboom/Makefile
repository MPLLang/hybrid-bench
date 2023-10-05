include ../config.mk

main.mpl: phony futtinykaboom
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futtinykaboom/tinykaboom.c glue.c

futtinykaboom: phony
	$(MAKE) -C futtinykaboom

clean: 
	rm main.mpl
	$(MAKE) -C futtinykaboom clean

.PHONY: phony
phony:
