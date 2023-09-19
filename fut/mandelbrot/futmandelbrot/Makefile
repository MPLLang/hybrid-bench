default: phony
	futhark cuda --library mandelbrot.fut
	~/proj/smlfut/smlfut --signature-name=FUTHARK_MANDELBROT --structure-name=FutharkMandelbrot -o . mandelbrot.json

clean: phony
	rm -f mandelbrot.c mandelbrot.h mandelbrot.json mandelbrot.sig mandelbrot.sml

.PHONY: phony
phony:
