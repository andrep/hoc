include ../../config.mk

all: Browser.app

Browser.app: Browser
	hocwrap Browser
	
Browser: *.hs
	mkdir -p build
	ghc --make -fglasgow-exts Main.hs -odir build -hidir build -O -o Browser 

interpret:
	mkdir -p build
	hocwrap -i -- -fglasgow-exts Main.hs -odir build -hidir build

zerolink: nolink interpret
	
nolink:
	mkdir -p build
	ghc --make -fglasgow-exts Main.hs -odir build -hidir build -O -pgml true

clean:
	rm -rf build/ Browser.app/ 'Interpreted Haskell Application.app/'