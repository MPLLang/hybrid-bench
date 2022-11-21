NVFLAGS = -O3
OUTDIR = objs

GOBJS =  $(OUTDIR)/blas_library.o $(OUTDIR)/allocate.o $(OUTDIR)/int_sequence.o $(OUTDIR)/mandel.o \
				$(OUTDIR)/float_sequence.o $(OUTDIR)/int_tuple_sequence.o


gpublob.o : $(OUTDIR) $(GOBJS)
	ld -r $(OUTDIR)/*.o -o gpublob.o
objs : 
	mkdir -p $(OUTDIR)
$(OUTDIR)/blas_library.o : userkernels/blas_library.cu
	nvcc -lcublas -c -o $(OUTDIR)/blas_library.o userkernels/blas_library.cu $(NVFLAGS) -llapack -lblas
$(OUTDIR)/allocate.o : cudalib/allocate.cu
	nvcc -c -o $(OUTDIR)/allocate.o cudalib/allocate.cu $(NVFLAGS)
$(OUTDIR)/int_sequence.o : cudalib/int_sequence.cu
	nvcc -c -o $(OUTDIR)/int_sequence.o cudalib/int_sequence.cu $(NVFLAGS)
$(OUTDIR)/int_tuple_sequence.o : cudalib/int_tuple_sequence.cu
	nvcc -c -o $(OUTDIR)/int_tuple_sequence.o cudalib/int_tuple_sequence.cu $(NVFLAGS)
$(OUTDIR)/float_sequence.o : cudalib/float_sequence.cu
	nvcc -c -o $(OUTDIR)/float_sequence.o cudalib/float_sequence.cu $(NVFLAGS)
$(OUTDIR)/mandel.o : userkernels/mandel.cu
	nvcc -c -o $(OUTDIR)/mandel.o userkernels/mandel.cu $(NVFLAGS)

clean : 
	(rm *.o;rm -rf objs)
