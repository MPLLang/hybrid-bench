MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-cc-opt '-I/opt/nvidia/cuda/include/' \
	-verbose 2 \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'

MPL=mpl

test.mpl: phony scan.c
	$(MPL) $(MLTONFLAGS) -output test.mpl test.mlb scan.c glue.c

scan.c: phony
	futhark cuda --library scan.fut

clean: 
	rm test.mpl

.PHONY: phony
phony:
