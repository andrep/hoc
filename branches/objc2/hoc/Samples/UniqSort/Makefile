include ../../config.mk

dist_srcdir = Samples/UniqSort
dist_FILES = \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: uniqsort

uniqsort: UniqSort.hs
	$(GHC) -o $@ --make $<
	
uniqsort.stripped: UniqSort.hs
	$(GHC) -o $@ --make -optl-Wl,-dead_strip $<
	
clean:
	rm -f uniqsort *.o *.hi

