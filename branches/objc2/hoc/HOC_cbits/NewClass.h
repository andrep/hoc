#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

struct hoc_ivar_list;

void newClass(struct objc_class * super_class,
                const char * name,                          /* never deallocate this */
				struct hoc_ivar_list *ivars,
				struct objc_method_list *methods,           /* never deallocate this */
				struct objc_method_list *class_methods);    /* never deallocate this */
				
