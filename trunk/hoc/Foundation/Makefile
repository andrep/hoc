include ../config.mk

all: libHSFoundation.a HSFoundation.o

ghcmake:
	ln -sf ../Bindings/ifgen-output/Foundation.hs .
	ln -sf ../Bindings/ifgen-output/Foundation .
	mkdir -p build/objects
	mkdir -p build/imports
	ghc --make Foundation.hs	\
		-package-name Foundation	\
		-odir build/objects \
		-hidir build/imports	\
		-package-conf ../HOC/HOC-$(PLATFORM).conf-inplace \
		-fglasgow-exts

HSFoundation.o: ghcmake
	find build/objects/ -name \*.o | xargs ld -r -x -o HSFoundation.o
#libHSFoundation.a: ghcmake
#	find build/objects/ -name \*.o | xargs libtool -static -o libHSFoundation.a	
libHSFoundation.a: HSFoundation.o
	libtool -static -o libHSFoundation.a HSFoundation.o

clean:
	rm -rf build libHSFoundation.a HSFoundation.o Foundation.hs Foundation
	
install: all
	mkdir -p `ghc --print-libdir`/Foundation
	cp -R libHSFoundation.a HSFoundation.o build/imports \
		`ghc --print-libdir`/Foundation/
	ranlib `ghc --print-libdir`/Foundation/libHSFoundation.a
	ghc-pkg --update-package	\
			--input-file=Foundation.conf
