default: phony lib
	futhark cuda --library sort.fut

lib: futhark.pkg
	futhark pkg sync

clean: phony
	rm -f sort.c sort.h sort.json

.PHONY: phony
phony:
