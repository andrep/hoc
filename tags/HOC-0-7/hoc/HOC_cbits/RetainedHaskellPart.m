#include <assert.h>
#include <stdlib.h>
#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-class.h>
#endif
#include "RetainedHaskellPart.h"

#ifdef GNUSTEP
static struct objc_ivar *
object_findInstanceVar(id obj, const char *name)
{
    Class cls = obj->class_pointer;
    struct objc_ivar *ivar = NULL;
    
    while(cls)
    {
        if(cls->ivars)
        {
            int i;
            
            for(i=0;i<cls->ivars->ivar_count;i++)
            {
                if(!strcmp(cls->ivars->ivar_list[i].ivar_name, name))
                    return &cls->ivars->ivar_list[i];
            }
        }
        cls = cls->super_class;
    }
    return NULL;
}

static struct objc_ivar *
object_getInstanceVariable(id obj, const char *name, void** out)
{
    struct objc_ivar *ivar = object_findInstanceVar(obj,name);
    if(ivar)
        *out = *(void**) ((char*)obj + ivar->ivar_offset);
    return ivar;
}

static struct objc_ivar *
object_setInstanceVariable(id obj, const char *name, void* val)
{
    struct objc_ivar *ivar = object_findInstanceVar(obj,name);
    if(ivar)
        *(void**) ((char*)obj + ivar->ivar_offset) = val;
    return ivar;
}
#endif

void* getRetainedHaskellPart(id obj)
{
    void *haskellPart = NULL;
    struct objc_ivar *ivar = object_getInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            &haskellPart );
    assert(ivar);
    return haskellPart;
}

void setRetainedHaskellPart(id obj, void* haskellPart)
{
    struct objc_ivar *ivar = object_setInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            haskellPart );
    assert(ivar);
}
