#include <objc/objc.h>

#ifdef __OBJC__
@class NSAutoreleasePool;
#else
typedef void NSAutoreleasePool;
#endif

void retainObject(id obj);
void releaseObject(id obj);
void deallocObject(id obj);

void autoreleaseObject(id obj);
NSAutoreleasePool *newAutoreleasePool();
