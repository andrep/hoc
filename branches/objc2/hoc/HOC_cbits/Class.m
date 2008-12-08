#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif
#include "Selector.h"

id getClassByName(const char* name)
{
#ifdef GNUSTEP
	return objc_get_class(name);
#else
	return objc_getClass(name);
#endif
}

Class getSuperClassForObject(id self)
{
#ifdef GNUSTEP
    return self->class_pointer->super_class;
#elif defined(__OBJC2__)
    return class_getSuperclass(object_getClass(self));
#else
    return self->isa->super_class;
#endif
}