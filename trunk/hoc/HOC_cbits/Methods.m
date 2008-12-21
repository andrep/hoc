#include <stdlib.h>
#include "Methods.h"
#include "Statistics.h"

#ifdef __OBJC__
#import <Foundation/NSException.h>
#endif

static void objcIMP(ffi_cif *cif, void * ret, void **args, void *userData)
{
    recordHOCEvent(kHOCAboutToEnterHaskell, args);
    NSException *e = (*(haskellIMP)userData)(cif, ret, args);
    recordHOCEvent(kHOCLeftHaskell, args);
    if(e != nil)
        [e raise];
}

static ffi_closure *newIMP(ffi_cif *cif, haskellIMP imp)
{
    ffi_closure *closure = (ffi_closure*) calloc(1, sizeof(ffi_closure));
    ffi_prep_closure(closure, cif, &objcIMP, (void*) imp);
    return closure;
}

struct hoc_method_list * makeMethodList(int n)
{
    struct hoc_method_list *list = 
        calloc(1, sizeof(struct hoc_method_list)
                  + (n-1) * sizeof(struct hoc_method));
    list->method_count = n;
    return list;
}

void setMethodInList(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,
        ffi_cif *cif,
        haskellIMP imp
    )
{   
    setMethodInListWithIMP(list, i, sel, types, (IMP) newIMP(cif, imp) );
}

void setMethodInListWithIMP(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,
        IMP imp
    )
{
    list->method_list[i].method_name = sel;
    list->method_list[i].method_types = types;
    list->method_list[i].method_imp = imp;
}

#ifndef __OBJC2__

/* Was previously makeMethodList */
static struct objc_method_list * makeObjcMethodList(int n)
{
    struct objc_method_list *list = 
        calloc(1, sizeof(struct objc_method_list)
                  + (n-1) * sizeof(struct objc_method));
    list->method_count = n;
    return list;
}

/* Was previously setMethodInList */
static void setObjCMethodInList(
        struct objc_method_list *list,
        int i,
        SEL sel,
        char *types,
        IMP imp
    )
{
#ifdef GNUSTEP
    list->method_list[i].method_name = (SEL) sel_get_name(sel);
#else
    list->method_list[i].method_name = sel;
#endif
    list->method_list[i].method_types = types;
    list->method_list[i].method_imp = imp;
}

struct objc_method_list *
convertMethodList(struct hoc_method_list * list) {
    struct objc_method_list * newList = makeObjcMethodList(list->method_count);
    int i;
    
    for(i = 0; i < list->method_count; i++)
    {
        struct hoc_method * method = &list->method_list[i];
        
        setObjCMethodInList(newList, i, method->method_name, method->method_types, method->method_imp);
    }
    
    return newList;
}

#endif // ifndef __OBJC2__