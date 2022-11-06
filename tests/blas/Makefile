MLTONFLAGS = -default-ann 'allowFFI true' -default-ann 'allowLineComments true' -link-opt \
						 '-rdynamic -L/usr/local/cuda-11.7/lib64/ -lcublas -lcudart -lstdc++'
LIBPATH = ../../lib/
BLOB = $(LIBPATH)/gpublob.o

blas : blas.sml $(BLOB)
	mlton $(MLTONFLAGS) blas.mlb ../../lib/gpublob.o
$(LIBPATH)/gpublob.o :
	(cd ../../lib/;make)

clean : 
	rm blas
