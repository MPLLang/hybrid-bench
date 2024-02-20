include ../config.mk

main.mpl.bin: phony futray
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb ../common/timer.c futray/ray.c futray/ray.smlfut.c

main.mlton.bin: phony futray
	$(MLTON) $(MLTONFLAGS) -output main.mlton.bin main.mlton.mlb ../common/timer.c futray/ray.c futray/ray.smlfut.c

futray: phony
	$(MAKE) -C futray

clean:
	rm -f main.mpl.bin
	$(MAKE) -C futray clean

.PHONY: phony
phony:
