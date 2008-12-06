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

struct objc_method_list * makeMethodList(int n);
void setMethodInList(
        struct objc_method_list *list,
        int i,
        SEL sel,
        char *types,    /* never deallocate this */
        ffi_cif *cif,   /* never deallocate this */
        haskellIMP imp
    );
