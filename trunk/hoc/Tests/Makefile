check: static dynamic

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

clean:
	rm -rf build
	rm -f TestFoundation.static
	rm -f TestFoundation.dynamic

