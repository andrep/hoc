include ../../config.mk

dist_srcdir = Samples/UniqSort
dist_FILES = \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: uniqsort

uniqsort: UniqSort.hs
	ghc -o $@ --make $<
	
clean:
	-rm uniqsort *.o *.hi

