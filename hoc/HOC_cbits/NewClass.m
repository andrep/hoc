#include <stdlib.h>
#include <Foundation/NSException.h>
#include <assert.h>
#include "NewClass.h"

void newClass(struct objc_class * super_class,
                const char * name,
				int instance_size,
				struct objc_ivar_list *ivars,
				struct objc_method_list *methods,
				struct objc_method_list *class_methods)
{
	struct objc_class * meta_class;
	struct objc_class * new_class;
	struct objc_class * root_class;
	int i;
	
	assert(objc_lookUpClass(name) == nil);
	
	for(root_class = super_class;
		root_class->super_class != nil;
		root_class = root_class->super_class)
		;
		
	new_class = calloc( 2, sizeof(struct objc_class) );
	meta_class = &new_class[1];
	
	new_class->isa      = meta_class;
	new_class->info     = CLS_CLASS;
	meta_class->info    = CLS_META;

	new_class->name = name;
	meta_class->name = name;
	
	new_class->instance_size = super_class->instance_size + instance_size;
	for(i=0; i<ivars->ivar_count; i++)
		ivars->ivar_list[i].ivar_offset += super_class->instance_size;
		
	new_class->ivars = ivars;
	
	new_class->methodLists = calloc( 1, sizeof(struct objc_method_list *) );
	meta_class->methodLists = calloc( 1, sizeof(struct objc_method_list *) );
    new_class->methodLists[0] = (struct objc_method_list*) -1;
    meta_class->methodLists[0] = (struct objc_method_list*) -1;

	new_class->super_class  = super_class;
	meta_class->super_class = super_class->isa;
	meta_class->isa         = (void *)root_class->isa;
	
	objc_addClass( new_class );
	
	class_addMethods(new_class, methods);
	class_addMethods(meta_class, class_methods);
}

static void objcIMP(ffi_cif *cif, void * ret, void **args, void *userData)
{
    NSException *e = (*(haskellIMP)userData)(cif, ret, args);
    if(e != nil)
        [e raise];
}

static ffi_closure *newIMP(ffi_cif *cif, haskellIMP imp)
{
    ffi_closure *closure = (ffi_closure*) calloc(1, sizeof(ffi_closure));
    ffi_prep_closure(closure, cif, &objcIMP, (void*) imp);
    return closure;
}

struct objc_method_list * makeMethodList(int n)
{
    struct objc_method_list *list = 
        calloc(1, sizeof(struct objc_method_list)
                  + (n-1) * sizeof(struct objc_method));
    list->method_count = n;
    return list;
}

void setMethodInList(
        struct objc_method_list *list,
        int i,
        SEL sel,
        char *types,
        ffi_cif *cif,
        haskellIMP imp
    )
{
    list->method_list[i].method_name = sel;
    list->method_list[i].method_types = types;
    list->method_list[i].method_imp = (IMP) newIMP(cif, imp);
}

struct objc_ivar_list * makeIvarList(int n)
{
    struct objc_ivar_list *list = 
        calloc(1, sizeof(struct objc_ivar_list)
                  + (n-1) * sizeof(struct objc_ivar));
    list->ivar_count = n;
    return list;
}

void setIvarInList(
        struct objc_ivar_list *list,
        int i,
        char *name,
        char *type,
        int offset
    )
{
    list->ivar_list[i].ivar_name = name;
    list->ivar_list[i].ivar_type = type;
    list->ivar_list[i].ivar_offset = offset;
}
