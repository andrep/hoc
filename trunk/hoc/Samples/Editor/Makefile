include ../../config.mk

dist_srcdir = Samples/Editor
dist_FILES = \
	Contents \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: Editor.app

Editor.app: Editor
	hocwrap Editor

Editor: *.hs
	mkdir -p build
	$(GHC) --make -fglasgow-exts  -fth Main.hs -odir build -hidir build -O -o Editor 

interpret:
	mkdir -p build
	hocwrap -i -- -fglasgow-exts Main.hs -odir build -hidir build

zerolink: nolink interpret

nolink:
	mkdir -p build
	$(GHC) --make -fglasgow-exts  -fth Main.hs -odir build -hidir build -O -pgml true
clean:
	rm -rf build Editor Editor.app 'Interpreted Haskell Application.app/'
