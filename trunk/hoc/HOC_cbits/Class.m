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

Class getSuperclassForClass(Class class)
{
#ifdef GNUSTEP
    if(CLS_ISRESOLV(class))
        return class->super_class;
    else
        return getClassByName((const char*) class->super_class);
        
#elif defined(__OBJC2__)
    return class_getSuperclass(class);
#else
    return class->super_class;
#endif
}

Class getRootClassForClass(Class super_class)
{
    Class root_class;
    
    for(root_class = super_class;
    getSuperclassForClass(root_class) != nil;
    root_class = getSuperclassForClass(root_class))
    ;
    
    return root_class;
}

Class getClassForObject(id object)
{
#ifdef __OBJC2__
    return object_getClass(object);
#else
    return object->isa;
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