#include "MemoryManagement.h"

#if 0

#include <Foundation/NSAutoreleasePool.h>

void retainObject(id obj)
{
	[obj retain];
}

void releaseObject(id obj)
{
	[obj release];
}

void deallocObject(id obj)
{
	[obj dealloc];
}

void autoreleaseObject(id obj)
{
	[obj autorelease];
}

NSAutoreleasePool *newAutoreleasePool()
{
	return [[NSAutoreleasePool alloc] init];
}

#else

#include "Selector.h"
#include "Class.h"
/* attempt to get it to work in GHCi by avoiding Objective C syntax
   (GHCi can't handle objc sections yet) */

#ifdef GNUSTEP
#define objc_msgSend(self,sel) (*objc_msg_lookup(self,sel))(self,sel)
#endif

static SEL selRetain = 0;
static SEL selRelease = 0;
static SEL selDealloc = 0;
static SEL selAutorelease = 0;
static SEL selAlloc = 0;
static SEL selInit = 0;
static Class clsNSAutoreleasePool = 0;

void retainObject(id obj)
{
    if(!selRetain)
        selRetain = getSelectorForName("retain");
    objc_msgSend(obj,selRetain);
}

void releaseObject(id obj)
{
    if(!selRelease)
        selRelease = getSelectorForName("release");
    objc_msgSend(obj,selRelease);
}

void deallocObject(id obj)
{
    if(!selDealloc)
        selDealloc = getSelectorForName("dealloc");
    objc_msgSend(obj,selDealloc);
}

void autoreleaseObject(id obj)
{
    if(!selAutorelease)
        selAutorelease = getSelectorForName("autorelease");
    objc_msgSend(obj,selAutorelease);
}

NSAutoreleasePool *newAutoreleasePool()
{
    if(!selAlloc)
        selAlloc = getSelectorForName("alloc");
    if(!selInit)
        selInit = getSelectorForName("init");
    if(!clsNSAutoreleasePool)
        clsNSAutoreleasePool = getClassByName("NSAutoreleasePool");
    
    NSAutoreleasePool *pool = objc_msgSend(clsNSAutoreleasePool,selAlloc);
    pool = objc_msgSend(pool, selInit);
    return pool;
}

#endif
