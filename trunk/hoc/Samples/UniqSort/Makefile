include ../../config.mk

dist_srcdir = Samples/UniqSort
dist_FILES = \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: uniqsort

uniqsort: UniqSort.hs
	ghc -o $@ --make $<
	
uniqsort.stripped: UniqSort.hs
	ghc -o $@ --make -optl-Wl,-dead_strip $<
	
clean:
	-rm uniqsort *.o *.hi

