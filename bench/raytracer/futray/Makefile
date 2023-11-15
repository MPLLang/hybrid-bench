include ../../config.mk

default: phony
	futhark $(FUTHARK_BACKEND) --library ray.fut
	smlfut ray.json --output-dir . --signature-name FUTHARK --structure-name Futhark --poly-arrays

clean: phony
	rm -f ray.c ray.h ray.json

.PHONY: phony
phony:
