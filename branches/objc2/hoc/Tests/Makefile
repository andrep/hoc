.PHONY: thtests cleanthbuild

check: static

static: TestFoundation.static
	./TestFoundation.static

dynamic: TestFoundation.dynamic
	./TestFoundation.dynamic


build:
	mkdir -p build/dynamic/imports
	mkdir -p build/dynamic/objects
	mkdir -p build/static/imports
	mkdir -p build/static/objects

TestFoundation.dynamic: build TestFoundation.hs
	ghc -dynamic -odir build/dynamic/objects -hidir build/dynamic/imports \
	 -o $@ -package-conf ../inplace.conf \
	 -package Foundation --make TestFoundation.hs

TestFoundation.static: build TestFoundation.hs
	ghc -odir build/static/objects -hidir build/static/imports \
	 -o $@  -package-conf ../inplace.conf \
	 -package Foundation --make TestFoundation.hs

thbuild:
	mkdir -p thbuild/static/imports
	mkdir -p thbuild/static/objects

cleanthbuild:
	rm -rf thbuild

TestHOCDeclareSelector: thbuild TestHOCDeclareSelector.hs TestHOCDeclareSelectorForward.hs
	ghc -odir thbuild/static/objects -hidir thbuild/static/imports \
	 -pgml true  -fth -o $@ -package-conf ../inplace.conf \
	 -package HOC --make TestHOCDeclareSelector.hs \
	 -main-is TestHOCDeclareSelector

thtests: cleanthbuild TestHOCDeclareSelector

clean: cleanthbuild
	rm -rf build
	rm -f TestFoundation.static
	rm -f TestFoundation.dynamic

