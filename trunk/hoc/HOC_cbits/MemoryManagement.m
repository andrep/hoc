#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

#include <stdlib.h>

#include "MemoryManagement.h"

#define DO_LOG 0

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
#define objc_msgSendSuper(super,sel) (*objc_msg_lookup_super(super,sel))(super->self,sel)
#endif

static SEL selRetain = 0;
static SEL selRelease = 0;
static SEL selRetainCount = 0;
static SEL selDealloc = 0;
static SEL selAutorelease = 0;
static SEL selAlloc = 0;
static SEL selInit = 0;
static Class clsNSAutoreleasePool = 0;

void retainObject(id obj)
{
    if(!selRetain)
        selRetain = getSelectorForName("retain");
#if DO_LOG
    printf("retain %p, %p\n",obj,getClassForObject(obj));
#endif

    objc_msgSend(obj,selRetain);
}

void releaseObject(id obj)
{
    if(!selRelease)
        selRelease = getSelectorForName("release");
#if DO_LOG
    printf("release %p, %p\n",obj,getClassForObject(obj));
#endif

    objc_msgSend(obj,selRelease);
}

void retainSuper(id obj, Class cls)
{
    if(!selRetain)
        selRetain = getSelectorForName("retain");
    
#if DO_LOG
    printf("retain super %p, %p\n",obj,cls);
#endif

    struct objc_super * super = calloc(1, sizeof(struct objc_super));
    
#if GNUSTEP
    super->self = obj;
    super->class = cls;
#else
    super->receiver = obj;
    super->super_class = cls;
#endif
    
    objc_msgSendSuper(super, selRetain);
}

void releaseSuper(id obj, Class cls)
{
    if(!selRelease)
        selRelease = getSelectorForName("release");
    
#if DO_LOG
    printf("release super %p, %p\n",obj,cls);
#endif

    struct objc_super * super = calloc(1, sizeof(struct objc_super));
    
#if GNUSTEP
    super->self = obj;
    super->class = cls;
#else
    super->receiver = obj;
    super->super_class = cls;
#endif
    
    objc_msgSendSuper(super, selRelease);
}

unsigned int retainCount(id obj) {
    unsigned int rc;
    
#if DO_LOG
    printf("retainCount %p = ",obj);
#endif
    if(!selRetainCount)
        selRetainCount = getSelectorForName("retainCount");
    
    rc = (unsigned int) objc_msgSend(obj,selRetainCount);
#if DO_LOG
    printf("%d\n",rc);
#endif
    return rc;
}

void deallocObject(id obj)
{
    if(!selDealloc)
        selDealloc = getSelectorForName("dealloc");
#if DO_LOG
    printf("dealloc %p\n",obj);
#endif
    objc_msgSend(obj,selDealloc);
}

void autoreleaseObject(id obj)
{
    if(!selAutorelease)
        selAutorelease = getSelectorForName("autorelease");
#if DO_LOG
    printf("autorelease %p\n",obj);
#endif
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

void releaseObjectWithPool(id obj)
{
    NSAutoreleasePool *pool = newAutoreleasePool();
    releaseObject(obj);
    releaseObject(pool);
}

void deallocObjectWithPool(id obj)
{
    NSAutoreleasePool *pool = newAutoreleasePool();
    deallocObject(obj);
    releaseObject(pool);
}
