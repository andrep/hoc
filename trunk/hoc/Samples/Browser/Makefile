include ../../config.mk

dist_srcdir = Samples/Browser
dist_FILES = \
	Contents \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: Browser.app

Browser.app: Browser Contents/Resources/all-selectors.txt
	hocwrap Browser

Browser: *.hs
	mkdir -p build
	$(GHC) --make -fglasgow-exts Main.hs -odir build -hidir build -O -o Browser

interpret:
	mkdir -p build
	hocwrap -i -- -fglasgow-exts Main.hs -odir build -hidir build

zerolink: nolink interpret

nolink:
	mkdir -p build
	$(GHC) --make -fglasgow-exts Main.hs -odir build -hidir build -O -pgml true

Contents/Resources/all-selectors.txt: ../../Bindings/all-selectors.txt
	cp $< $@

clean:
	rm -rf build/ Browser Browser.app/ 'Interpreted Haskell Application.app/' Contents/Resources/all-selectors.txt
	
