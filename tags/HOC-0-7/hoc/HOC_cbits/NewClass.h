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

void newClass(struct objc_class * super_class,
                const char * name,                          /* never deallocate this */
				int instance_size,
				struct objc_ivar_list *ivars,               /* never deallocate this */
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

struct objc_ivar_list * makeIvarList(int n);
void setIvarInList(
        struct objc_ivar_list *list,
        int i,
        char *name,     /* never deallocate this */
        char *type,     /* never deallocate this */
        int offset
    );
