include ../config.mk

SRCS=\
	Class.m\
	GetNewHaskellData.m\
	MemoryManagement.m\
	ObjectMap.m\
	Selector.m\
	FFICallInterface.m\
	Invocation.m\
	NewClass.m\
	RetainedHaskellPart.m\
	Marshalling.m
OBJS=$(SRCS:%.m=%.o)

CFLAGS=-g $(FOUNDATION_INCLUDES) -I../libffi-src/build/include

all:	libHOC_cbits.a HOC_cbits.o

clean:
		rm -rf libHOC_cbits.a HOC_cbits.o $(OBJS) depend

HOC_cbits.o: libHOC_cbits.a
		ld -r -x -o HOC_cbits.o $(ALL_LOAD) libHOC_cbits.a

libHOC_cbits.a:	$(OBJS)
		$(MAKE_STATIC_LIB) libHOC_cbits.a $(OBJS) ../libffi-src/build/.libs/libffi.a

# %.o:	%.m %.h
#		gcc -c $(CFLAGS) -o $@ $<
		
depend: $(SRCS)
	cc -MM $(CFLAGS) $(SRCS) > depend
		
install: all
	mkdir -p `ghc --print-libdir`/HOC
	cp -R libHOC_cbits.a HOC_cbits.o \
		`ghc --print-libdir`/HOC/
	ranlib `ghc --print-libdir`/HOC/libHOC_cbits.a
	
-include depend
