default: phony
	futhark cuda --library ray.fut
	smlfut ray.json --output-dir . --signature-name RAY --structure-name Ray --poly-arrays

clean: phony
	rm -f ray.c ray.h ray.json

.PHONY: phony
phony:
