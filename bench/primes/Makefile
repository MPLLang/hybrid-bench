include ../config.mk

main.mpl.bin: phony futhark-primes
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark-primes.c futhark-primes.smlfut.c

main.mlton.bin: phony futhark-primes
	$(MLTON) $(MLTONFLAGS) -output main.mlton.bin main.mlton.mlb futhark-primes.c futhark-primes.smlfut.c

futhark-primes: phony
	futhark $(FUTHARK_BACKEND) --library futhark-primes.fut
	smlfut -o . --signature-name=FUTHARK_PRIMES --structure-name=FutharkPrimes --poly-arrays futhark-primes.json

clean: 
	rm test.mpl

.PHONY: phony
phony:
