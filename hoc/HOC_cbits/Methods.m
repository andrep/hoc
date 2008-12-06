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

struct objc_method_list * makeMethodList(int n)
{
    struct objc_method_list *list = 
        calloc(1, sizeof(struct objc_method_list)
                  + (n-1) * sizeof(struct objc_method));
    list->method_count = n;
    return list;
}

void setMethodInList(
        struct objc_method_list *list,
        int i,
        SEL sel,
        char *types,
        ffi_cif *cif,
        haskellIMP imp
    )
{
#ifdef GNUSTEP
    list->method_list[i].method_name = (SEL) sel_get_name(sel);
#else
    list->method_list[i].method_name = sel;
#endif
    list->method_list[i].method_types = types;
    list->method_list[i].method_imp = (IMP) newIMP(cif, imp);
}
