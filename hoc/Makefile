SUBDIRS=\
	HOC_cbits			\
	HOC					\
	InterfaceGenerator	\
	Bindings			\
	Foundation			\
	AppKit				\
	Tools



all:
	for dir in $(SUBDIRS); do (cd $$dir; make all) done
	
clean:
	for dir in $(SUBDIRS); do (cd $$dir; make clean) done
	
install:
	for dir in $(SUBDIRS); do (cd $$dir; make install) done
