PLATFORM=gnustep
ALL_LOAD=--whole-archive
MAKE_STATIC_LIB=ar cs
FOUNDATION_INCLUDES=-I/usr/GNUstep/System/Library/Headers/
FOUNDATION_LIBS=-L/usr/GNUstep/System/Library/Libraries -lgnustep-base -lobjc
