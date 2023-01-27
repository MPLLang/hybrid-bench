MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-cc-opt '-I/opt/nvidia/cuda/include/' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=mpl

test.mpl: phony sieve.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb sieve.c glue.c

sieve.c: phony
	futhark cuda --library sieve.fut

clean: 
	rm test.mpl

.PHONY: phony
phony:
