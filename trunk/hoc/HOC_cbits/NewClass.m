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

static Class allocateClassPair(Class super_class, const char * name) {
#ifdef __OBJC2__
    return objc_allocateClassPair(super_class, name, 0);
#else
    Class new_class = calloc( 2, sizeof(struct objc_class) );
    Class meta_class = &new_class[1];
	Class root_class = getRootClassForClass(super_class);

    new_class->isa      = meta_class;
    new_class->info     = CLS_CLASS;
    meta_class->info    = CLS_META;
    
    new_class->name = name;
    meta_class->name = name;
    
#   ifdef GNUSTEP
        new_class->super_class = (void*)(super_class->name);
        meta_class->super_class = (void*)(super_class->isa->name);
#   else
        new_class->super_class  = super_class;
        meta_class->super_class = super_class->isa;
        meta_class->isa         = (void *)root_class->isa;
#   endif
    
    return new_class;
#endif
}

static void registerClassPair(Class new_class) {
#ifdef GNUSTEP
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
#elif defined(__OBJC2__)
    objc_registerClassPair(new_class);
#else
    objc_addClass( new_class );
#endif
}

static void addIvarsToClass(Class new_class, struct hoc_ivar_list *ivars)
{
#ifdef __OBJC2__
    int i;
    
    for (i = 0; i < ivars->ivar_count; i++)
    {
        struct hoc_ivar *ivar = &ivars->ivar_list[i];
        class_addIvar(new_class, ivar->ivar_name,
            ivar->ivar_size, ivar->ivar_alignment, ivar->ivar_types);
    }
#else
    Class super_class = getSuperclassForClass(new_class);
    
    int instance_size;
    new_class->ivars = buildIndexedIvarList(
                                ivars, 
                                super_class->instance_size, 
                                &instance_size);
    
    new_class->instance_size = super_class->instance_size + instance_size;
#endif
}

static void addMethodsToClass(Class new_class, struct hoc_method_list *methods)
{
#ifdef GNUSTEP
    class_add_method_list(new_class, convertMethodList(methods));
#elif defined(__OBJC2__)
    int i;
    for (i = 0; i < methods->method_count; i++)
    {
        struct hoc_method * m = &methods->method_list[i];
        class_addMethod(new_class, m->method_name, m->method_imp, m->method_types);
    }
#else
    new_class->methodLists = calloc( 1, sizeof(struct objc_method_list *) );
    new_class->methodLists[0] = (struct objc_method_list*) -1;
    
    class_addMethods(new_class, convertMethodList(methods));
#endif
}

void newClass(Class super_class,
                const char * name,
                struct hoc_ivar_list *ivars,
                struct hoc_method_list *methods,
                struct hoc_method_list *class_methods)
{
	Class meta_class;
	Class new_class;
	
	assert(objc_lookUpClass(name) == nil);
	
	/* Allocate the class and metaclass */
    new_class = allocateClassPair(super_class, name);
    meta_class = getClassForObject(new_class);
	
	/* Add instance variables to the class */
    addIvarsToClass(new_class, ivars);
    
    /* Add methods and class methods */
    /* I don't know whether order actually matters here in the non-objc2 cases,
       so I'm leaving it as it was. */
#ifdef __OBJC2__
    addMethodsToClass(new_class, methods);
    addMethodsToClass(meta_class, class_methods);
    
    registerClassPair(new_class);
#else
    registerClassPair(new_class);
    
    addMethodsToClass(new_class, methods);
    addMethodsToClass(meta_class, class_methods);
#endif

}

