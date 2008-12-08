#include <stdlib.h>
#include <Foundation/NSException.h>
#include <assert.h>
#include "Class.h"
#include "Ivars.h"
#include "Methods.h"
#include "NewClass.h"

#ifdef GNUSTEP
#define isa class_pointer
#define CLS_CLASS _CLS_CLASS
#define CLS_META _CLS_META
#endif

static struct objc_class * getSuper(struct objc_class *class)
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

void newClass(struct objc_class * super_class,
                const char * name,
                struct hoc_ivar_list *ivars,
                struct hoc_method_list *methods,
                struct hoc_method_list *class_methods)
{
	struct objc_class * meta_class;
	struct objc_class * new_class;
	
	assert(objc_lookUpClass(name) == nil);
	
	/* Allocate the class and metaclass */
#ifdef __OBJC2__
    new_class = objc_allocateClassPair(super_class, name, 0);
    meta_class = object_getClass(new_class);
#else
	new_class = calloc( 2, sizeof(struct objc_class) );
	meta_class = &new_class[1];
	
	new_class->isa      = meta_class;
	new_class->info     = CLS_CLASS;
	meta_class->info    = CLS_META;

	new_class->name = name;
	meta_class->name = name;
#endif
	
	/* Add instance variables to the class */
#ifdef __OBJC2__
    {
        int i;
        
        for (i = 0; i < ivars->ivar_count; i++)
        {
            struct hoc_ivar *ivar = &ivars->ivar_list[i];
            class_addIvar(new_class, ivar->ivar_name,
                ivar->ivar_size, ivar->ivar_alignment, ivar->ivar_types);
        }

    }
#else
    {
        int instance_size;
    	new_class->ivars = buildIndexedIvarList(
    	                            ivars, 
    	                            super_class->instance_size, 
                                    &instance_size);
        
        new_class->instance_size = super_class->instance_size + instance_size;
    }
#endif
    
    /* Add methods and class methods */
#ifdef GNUSTEP
	new_class->super_class = (void*)(super_class->name);
    meta_class->super_class = (void*)(super_class->isa->name);
	
    {
    	Module_t module = calloc(1, sizeof(Module));
        Symtab_t symtab = calloc(1, sizeof(Symtab) + sizeof(void*) /* two defs pointers */);
        extern void __objc_exec_class (Module_t module);
        extern void __objc_resolve_class_links ();
        
        module->version = 8;	
        module->size = sizeof(Module);
        module->name = strdup(name);
        module->symtab = symtab;
        symtab->cls_def_cnt = 1;
        symtab->defs[0] = new_class;
        symtab->defs[1] = NULL;
        
        __objc_exec_class (module);
        __objc_resolve_class_links();
    }
    
    class_add_method_list(new_class, convertMethodList(methods));
    class_add_method_list(meta_class, convertMethodList(class_methods));
#elif defined(__OBJC2__)
    {
        int i;
        for (i = 0; i < methods->method_count; i++)
        {
            struct hoc_method * m = &methods->method_list[i];
            class_addMethod(new_class, m->method_name, m->method_imp, m->method_types);
        }
        
        for (i = 0; i < class_methods->method_count; i++)
        {
            struct hoc_method * m = &class_methods->method_list[i];
            class_addMethod(meta_class, m->method_name, m->method_imp, m->method_types);
        }
        
        objc_registerClassPair(new_class);
    }
#else
    {
	    struct objc_class * root_class;
	    for(root_class = super_class;
	    	root_class->super_class != nil;
	    	root_class = getSuper(root_class))
	    	;
        
	    new_class->methodLists = calloc( 1, sizeof(struct objc_method_list *) );
	    meta_class->methodLists = calloc( 1, sizeof(struct objc_method_list *) );
        new_class->methodLists[0] = (struct objc_method_list*) -1;
        meta_class->methodLists[0] = (struct objc_method_list*) -1;
        
	    new_class->super_class  = super_class;
	    meta_class->super_class = super_class->isa;
	    meta_class->isa         = (void *)root_class->isa;
	    
	    objc_addClass( new_class );
	    
	    class_addMethods(new_class, convertMethodList(methods));
	    class_addMethods(meta_class, convertMethodList(class_methods));
    }
#endif

}

