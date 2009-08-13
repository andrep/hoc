#include <objc/objc.h>

#ifdef __OBJC__
@class NSAutoreleasePool;
#else
typedef void NSAutoreleasePool;
#endif

void retainObject(id obj);
void releaseObject(id obj);

void retainSuper(id obj, Class cls);
void releaseSuper(id obj, Class cls);
unsigned int retainCount(id obj);

void deallocObject(id obj);

void autoreleaseObject(id obj);
NSAutoreleasePool *newAutoreleasePool();

void releaseObjectWithPool(id obj);
void deallocObjectWithPool(id obj);
