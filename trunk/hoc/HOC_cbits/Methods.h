#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

#include <ffi.h>

#ifdef __OBJC__
@class NSException;
#else
typedef void NSException;
#endif

typedef NSException *(*haskellIMP)(
                        ffi_cif *cif,
                        void * ret,
                        void **args
                    );

struct hoc_method {
    SEL method_name;
    char *method_types;
    IMP method_imp;
};

struct hoc_method_list {
    int method_count;
    
    /* variable length structure */
    struct hoc_method method_list[1];
};

struct hoc_method_list * makeMethodList(int n);

void setMethodInListWithIMP(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,    /* never deallocate this */
        IMP imp         /* never deallocate this */
    );

void setMethodInList(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,    /* never deallocate this */
        ffi_cif *cif,   /* never deallocate this */
        haskellIMP imp
    );

#ifndef __OBJC2__

struct objc_method_list * convertMethodList(struct hoc_method_list * list);

#endif