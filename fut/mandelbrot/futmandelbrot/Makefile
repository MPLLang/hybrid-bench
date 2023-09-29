default: phony
	futhark cuda --library mandelbrot.fut
	smlfut --poly-arrays --signature-name=FUTHARK_MANDELBROT --structure-name=FutharkMandelbrot -o . mandelbrot.json

clean: phony
	rm -f mandelbrot.c mandelbrot.h mandelbrot.json mandelbrot.sig mandelbrot.sml

.PHONY: phony
phony:
