BENCHMARKS=mandelbrot raytracer primes quickhull build-intervaltree

all: $(BENCHMARKS:=/main.mpl.bin)

%/main.mpl.bin: %/*.sml ../dep/lib
	$(MAKE) -C $*

../dep/lib: ../dep/sml.pkg
	cd ../dep && smlpkg sync

clean:
	for d in $(BENCHMARKS); do $(MAKE) -C $$d clean; done
