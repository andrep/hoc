check: TestFoundation.dynamic TestFoundation.static
	./TestFoundation.dynamic
	./TestFoundation.static

build:
	mkdir -p build/dynamic/imports
	mkdir -p build/dynamic/objects
	mkdir -p build/static/imports
	mkdir -p build/static/objects

TestFoundation.dynamic: build TestFoundation.hs
	ghc -dynamic -odir build/dynamic/objects -hidir build/dynamic/imports \
	 -o $@ -fglasgow-exts -package-conf ../inplace.conf \
	 -package Foundation --make $<

TestFoundation.static: build TestFoundation.hs
	ghc -odir build/static/objects -hidir build/static/imports \
	 -o $@ -fglasgow-exts -package-conf ../inplace.conf \
	 -package Foundation --make $<

clean:
	-rm *.o *.hi TestFoundation

