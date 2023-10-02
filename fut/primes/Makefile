include ../config.mk

main.mpl.bin: phony
	futhark $(FUTHARK_BACKEND) --library futhark-primes.fut
	smlfut -o . --signature-name=FUTHARK_PRIMES --structure-name=FutharkPrimes --poly-arrays futhark-primes.json
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark-primes.c futhark-primes.smlfut.c

clean: 
	rm test.mpl

.PHONY: phony
phony:
