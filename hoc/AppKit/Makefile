include ../config.mk

all: libHSAppKit.a HSAppKit.o

ghcmake:
	ln -sf ../Bindings/ifgen-output/Cocoa.hs .
	ln -sf ../Bindings/ifgen-output/AppKit.hs .
	ln -sf ../Bindings/ifgen-output/AppKit .
	-test -d ../Bindings/ifgen-output/GNUstepGUI && \
		ln -sf ../Bindings/ifgen-output/GNUstepGUI .
	mkdir -p build/objects
	mkdir -p build/imports
	ghc --make AppKit.hs	\
		-package-name AppKit	\
		-odir build/objects \
		-hidir build/imports	\
		-package-conf ../HOC/HOC-$(PLATFORM).conf-inplace \
		-package-conf ../Foundation/Foundation.conf-inplace \
		-fglasgow-exts
	ghc -c Cocoa.hs \
		-package-name AppKit	\
		-ibuild/imports	   \
		-o build/objects/Cocoa.o	\
		-ohi build/imports/Cocoa.hi \
		-package-conf ../HOC/HOC-$(PLATFORM).conf-inplace \
		-package-conf ../Foundation/Foundation.conf-inplace \
		-fglasgow-exts

	
HSAppKit.o: ghcmake
	find build/objects/ -name \*.o | xargs ld -r -x -o HSAppKit.o
#libHSAppKit.a: ghcmake
#	find build/objects/ -name \*.o | xargs libtool -static -o libHSAppKit.a
libHSAppKit.a: HSAppKit.o
	libtool -static -o libHSAppKit.a HSAppKit.o

clean:
	rm -rf build libHSAppKit.a HSAppKit.o AppKit.hs AppKit Cocoa.hs

install: all
	mkdir -p `ghc --print-libdir`/AppKit
	cp -R libHSAppKit.a HSAppKit.o build/imports \
		`ghc --print-libdir`/AppKit/
	ranlib `ghc --print-libdir`/AppKit/libHSAppKit.a
	ghc-pkg --update-package	\
			--input-file=AppKit.conf
