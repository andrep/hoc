#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

#include <stdlib.h>
#include <assert.h>

#include "Ivars.h"

#ifdef GNUSTEP
struct objc_ivar *
class_getInstanceVariable(Class cls, const char *name)
{
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

struct objc_ivar *
object_getInstanceVariable(id obj, const char *name, void** out)
{
    struct objc_ivar *ivar = class_getInstanceVariable(obj->class_pointer,name);
    if(ivar)
        *out = *(void**) ((char*)obj + ivar->ivar_offset);
    return ivar;
}

struct objc_ivar *
object_setInstanceVariable(id obj, const char *name, void* val)
{
    struct objc_ivar *ivar = class_getInstanceVariable(obj->class_pointer,name);
    if(ivar)
        *(void**) ((char*)obj + ivar->ivar_offset) = val;
    return ivar;
}
#endif


struct hoc_ivar_list * makeIvarList(int n)
{
    struct hoc_ivar_list *list = 
        calloc(1, sizeof(struct hoc_ivar_list)
                  + (n-1) * sizeof(struct hoc_ivar));
    list->ivar_count = n;
    return list;
}

void setIvarInList(
        struct hoc_ivar_list *list,
        int i,
        char *name,
        char *types,
        size_t size,
        uint8_t alignment
    )
{
    list->ivar_list[i].ivar_name      = name;
    list->ivar_list[i].ivar_types     = types;
    list->ivar_list[i].ivar_size      = size;
    list->ivar_list[i].ivar_alignment = alignment;
}

#ifndef __OBJC2__

/* Used to be makeIvarList in NewClass.m */
static struct objc_ivar_list * makeIndexedIvarList(int n)
{
    struct objc_ivar_list *list = 
        calloc(1, sizeof(struct objc_ivar_list)
                  + (n-1) * sizeof(struct objc_ivar));
    list->ivar_count = n;
    return list;
}

/* Used to be setIvarInList in NewClass.m */
static void setIvarInIndexedList(
        struct objc_ivar_list *list,
        int i,
        char *name,
        char *type,
        int offset
    )
{
    list->ivar_list[i].ivar_name = name;
    list->ivar_list[i].ivar_type = type;
    list->ivar_list[i].ivar_offset = offset;
}

struct objc_ivar_list * buildIndexedIvarList(
        struct hoc_ivar_list *list,
        int start_offset,
        int *instance_size      /* out */
    )
{
    struct objc_ivar_list * outList = makeIndexedIvarList(list->ivar_count);
    int offset = start_offset;
    int i;
    
    for (i = 0; i < list->ivar_count; i++)
    {
        struct hoc_ivar *ivar = &list->ivar_list[i];
        
        int align = ivar->ivar_alignment;
        int alignmask = align - 1;
        
        assert((align & alignmask) == 0);
        if ((offset & alignmask) != 0)
            offset = (offset & ~alignmask) + align;
        
        setIvarInIndexedList(outList, i, ivar->ivar_name, ivar->ivar_types, offset);
        
        offset += ivar->ivar_size;
    }
    
    *instance_size = offset - start_offset;
    return outList;
}

#endif // ifndef __OBJC2__
