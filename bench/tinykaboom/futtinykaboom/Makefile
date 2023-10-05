include ../../config.mk

default: phony
	futhark $(FUTHARK_BACKEND) --library tinykaboom.fut

clean: phony
	rm -f tinykaboom.c tinykaboom.h tinykaboom.json

.PHONY: phony
phony:
