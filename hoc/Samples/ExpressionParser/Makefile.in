include ../../config.mk

dist_srcdir = Samples/ExpressionParser
dist_FILES = \
	Contents \
	$(wildcard *.hs) \
	Makefile.in \
	$(NULL)

all: ExpressionParser.app

ExpressionParser.app: ExpressionParser
	hocwrap ExpressionParser

ExpressionParser: *.hs
	mkdir -p build
	ghc --make Main.hs -odir build -hidir build -O -o $@

interpret:
	mkdir -p build
	hocwrap -i -- Main.hs -odir build -hidir build

zerolink: nolink interpret

nolink:
	mkdir -p build
	ghc --make Main.hs -odir build -hidir build -O -pgml true

clean:
	rm -rf build/ ExpressionParser ExpressionParser.app/ 'Interactive Haskell Application.app/'

