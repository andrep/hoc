#include <objc/objc.h>

id getClassByName(const char* name);

Class getSuperclassForClass(Class class);
Class getRootClassForClass(Class super_class);

Class getClassForObject(id self);
Class getSuperClassForObject(id self);