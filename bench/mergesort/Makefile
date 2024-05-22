include ../config.mk

FDIR=futhark

main.mpl.bin: phony $(FDIR)/sort.sml
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb $(FDIR)/sort.c $(FDIR)/sort.smlfut.c

main.mlton.bin: phony $(FDIR)/sort.sml
	$(MLTON) $(MLTONFLAGS) -output main.mlton.bin main.mlton.mlb $(FDIR)/sort.c $(FDIR)/sort.smlfut.c

$(FDIR)/sort.sml: $(FDIR)/sort.fut $(FDIR)/lib
	futhark $(FUTHARK_BACKEND) --library $(FDIR)/sort.fut
	smlfut -o $(FDIR) --poly-arrays  --structure=FutharkSort --signature=FUTHARK_SORT$(FDIR)/sort.json

$(FDIR)/lib:
	(cd $(FDIR); futhark pkg sync)

clean:
	rm -f $(FDIR)/sort.json $(FDIR)/sort.c $(FDIR)/sort.h $(FDIR)/sort.sml $(FDIR)/sort.sig \
	  $(FDIR)/sort.smlfut.c futhark.cache main.mpl.bin
	rm -rf $(FDIR)/lib

.PHONY: phony
phony:
