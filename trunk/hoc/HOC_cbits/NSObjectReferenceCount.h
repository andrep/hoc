// These two prototypes are normally in NSObject.h, but that header requires
// Objective-C, while we can use this one from GHC.

#include <objc/objc.h>

void NSIncrementExtraRefCount(id object);

BOOL NSDecrementExtraRefCountWasZero(id object);

unsigned NSExtraRefCount(id object);