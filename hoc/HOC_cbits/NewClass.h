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

struct hoc_ivar_list;

void newClass(struct objc_class * super_class,
                const char * name,                          /* never deallocate this */
				struct hoc_ivar_list *ivars,
				struct objc_method_list *methods,           /* never deallocate this */
				struct objc_method_list *class_methods);    /* never deallocate this */
				
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
