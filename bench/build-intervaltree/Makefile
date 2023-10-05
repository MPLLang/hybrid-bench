include ../config.mk

main.mpl.bin: phony futsort
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c futsort/sort.c glue.c

main.mpl: phony futsort
	$(MPL) $(MLTONFLAGS) -output main.mpl main.mlb ../common/timer.c futsort/sort.c glue.c

futsort: phony
	$(MAKE) -C futsort

clean:
	rm -f main.mpl.bin
	$(MAKE) -C futsort clean

.PHONY: phony
phony:
