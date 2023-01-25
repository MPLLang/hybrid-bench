MLTONFLAGS = -default-ann 'allowFFI true' -default-ann 'allowLineComments true' -link-opt \
						 '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcublas -lcudart -lstdc++'
LIBPATH = ../../lib/
BLOB = $(LIBPATH)/gpublob.o

MPL=mpl
#MPL=~/mpl/build/bin/./mpl

sieve.mpl : sieve.sml $(BLOB)
	$(MPL) $(MLTONFLAGS) sieve.mpl.mlb ../../lib/gpublob.o
$(LIBPATH)/gpublob.o :
	(cd ../../lib/;make)

clean : 
	rm sieve.mpl
