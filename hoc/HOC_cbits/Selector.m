#include "Selector.h"

SEL getSelectorForName(const char* name)
{
#ifdef GNUSTEP
	return sel_register_name(name);
#else
	return sel_registerName(name);
#endif
}
