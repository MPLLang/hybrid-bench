include ../../config.mk

default: phony lib
	futhark $(FUTHARK_BACKEND) --library sort.fut

lib: futhark.pkg
	futhark pkg sync

clean: phony
	rm -f sort.c sort.h sort.json

.PHONY: phony
phony:
