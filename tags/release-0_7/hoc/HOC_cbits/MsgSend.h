#include <objc/objc.h>

#ifndef GNUSTEP
/*
 * <objc/objc-runtime.h> contains a single line that is not pure C
 * code, so we have to duplicate the prototypes here for GHC's benefit.
*/

void objc_msgSend_stret(void * stretAddr, id self, SEL op, ...);
id objc_msgSend(id self, SEL op, ...);
#endif
