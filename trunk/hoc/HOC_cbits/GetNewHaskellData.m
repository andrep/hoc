#include <stdlib.h>
#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

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
    struct objc_method *m;

    IMP imp = 0;

    if(!isa)
        return 0;

    if(!selGetHaskellData)
        selGetHaskellData = getSelectorForName("__getHaskellData__");
    
#ifdef GNUSTEP
        // first, use objc_msg_lookup to make sure
        // that the objc runtime has inited everything
    objc_msg_lookup(obj, selGetHaskellData);
    
        // Now find the right method.
        // We don't want to use objc_msg_lookup_super because 
        // we don't want our message to be forwarded.
    m = class_get_instance_method(isa, selGetHaskellData);
#else
    m = class_getInstanceMethod(isa, selGetHaskellData);
#endif

    if(m)
        imp = m->method_imp;
    
    if(imp)
        return (*(getHaskellDataIMP)imp)(obj, selGetHaskellData);
    else
        return 0;
}


void *getNewHaskellData(id obj)
{
#ifdef GNUSTEP
    return getNewHaskellDataForClass(obj, obj->class_pointer);
#else
    return getNewHaskellDataForClass(obj, obj->isa);
#endif
}
