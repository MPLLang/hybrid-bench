MLTONFLAGS = -default-ann 'allowFFI true' -default-ann 'allowLineComments true' -link-opt \
						 '-rdynamic -L/usr/local/cuda-11.7/lib64/ -lcublas -lcudart -lstdc++'
LIBPATH = ../../lib/
BLOB = $(LIBPATH)/gpublob.o

blas_managed.mpl : blas_managed.sml $(BLOB)
	~/mpl/build/bin/./mpl $(MLTONFLAGS) blas_managed.mpl.mlb ../../lib/gpublob.o
$(LIBPATH)/gpublob.o :
	(cd ../../lib/;make)

clean : 
	rm blas_managed.mpl
