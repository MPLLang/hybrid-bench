default: phony
	futhark cuda --library ray.fut
	smlfut ray.json -o . --signature-name RAY --structure-name Ray

clean: phony
	rm -f ray.c ray.h ray.json

.PHONY: phony
phony:
