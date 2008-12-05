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
