#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

struct hoc_ivar_list;
struct hoc_method_list;

void newClass(Class super_class,
                const char * name,                          /* never deallocate this */
				struct hoc_ivar_list *ivars,
				struct hoc_method_list *methods,
				struct hoc_method_list *class_methods);
				
