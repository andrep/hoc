#include <stdlib.h>
#include <objc/objc-runtime.h>

#include "GetNewHaskellData.h"
#include "Selector.h"

/*
    Why don't we use normal Objective-C messaging?
    Because the object in question might not implement __getHaskellData__.
    
    Why not add __getHaskellData__ as a category to NSObject or use
    respondsToSelector?
    Because the object might not even descend from NSObject.
*/
    
typedef void * (*getHaskellDataIMP)(id target, SEL sel);

static SEL selGetHaskellData = 0;

void *getNewHaskellDataForClass(id obj, Class isa)
{
    Method m;
    IMP imp = 0;

    if(!selGetHaskellData)
        selGetHaskellData = getSelectorForName("__getHaskellData__");
    
    m = class_getInstanceMethod(isa, selGetHaskellData);
    
    if(m)
        imp = m->method_imp;
    
    if(imp)
        return (*(getHaskellDataIMP)imp)(obj, selGetHaskellData);
    else
        return 0;
}


void *getNewHaskellData(id obj)
{
    return getNewHaskellDataForClass(obj, obj->isa);
}
