PLATFORM=gnustep
ALL_LOAD=--whole-archive
MAKE_STATIC_LIB=ar cqs
FOUNDATION_INCLUDES=-I/usr/GNUstep/System/Library/Headers/
FOUNDATION_LIBS=-L/usr/GNUstep/System/Library/Libraries \
    -L/usr/lib/gcc-lib/i386-redhat-linux/3.2 \
    -lobjc -lgnustep-base
DEFINES=-DGNUSTEP
CFLAGS=$(FOUNDATION_INCLUDES) $(DEFINES) \
    -fconstant-string-class=NSConstantString    \
     -Wno-import
