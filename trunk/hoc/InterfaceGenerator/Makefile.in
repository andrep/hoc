include ../config.mk

all:
	mkdir -p build
	ghc --make Main.hs	\
		-odir build -hidir build \
		-package-conf ../HOC/HOC-$(PLATFORM).conf-inplace \
		-o ifgen
		
clean:
	rm -rf ifgen build

install: all
	echo "Interface Generator will be used in-place."
