MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-disable-pass splitTypes1 \
	-disable-pass splitTypes2 \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=/home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl

main.mpl.bin: phony
	futhark cuda --library futhark-primes.fut
	smlfut -o . --signature-name=FUTHARK_PRIMES --structure-name=FutharkPrimes futhark-primes.json
	$(MPL) $(MLTONFLAGS) -output main.mpl.bin main.mlb futhark-primes.c # futhark-primes.smlfut.c

clean: 
	rm test.mpl

.PHONY: phony
phony: