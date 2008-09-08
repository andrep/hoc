#include <objc/objc.h>
#include "NewClass.h"
#include "Class.h"
#include "Selector.h"
#include "Marshalling.h"
#include "HsFFI.h"

static BOOL excWrapperInited = NO;
static int stablePtrOffset;
static id clsHOCHaskellException;
static SEL selExceptionWithNameReasonUserInfo = 0;
static SEL selDealloc;
//static void initExceptionWrapper() __attribute__((constructor));

static void exc_dealloc(id self, SEL sel)
{
    HsStablePtr sp = * (HsStablePtr*) (((char*)self) + stablePtrOffset);
    struct objc_super super;

    hs_free_stable_ptr(sp);
    
#if GNUSTEP
    super.self = self;
    super.class = self->class_pointer->super_class;
    
    (*objc_msg_lookup_super(&super, selDealloc))(self, selDealloc);
#else
    super.receiver = self;
    super.class = self->isa->super_class;
    objc_msgSendSuper(&super, selDealloc);
#endif
}

static void initExceptionWrapper()
{
    if(!excWrapperInited)
    {
        struct objc_method_list *methods = makeMethodList(1);
        struct objc_method_list *class_methods = makeMethodList(0);
        struct objc_ivar_list *ivars = makeIvarList(1);
        
        selDealloc = getSelectorForName("dealloc");
        
#ifdef GNUSTEP
        methods->method_list[0].method_name = (SEL)"dealloc";
#else
        methods->method_list[0].method_name = selDealloc;
#endif
        methods->method_list[0].method_types = "v@:";
        methods->method_list[0].method_imp = (IMP) &exc_dealloc;
        
        setIvarInList(ivars, 0, "_haskellExecption", "^v", 0);
      
        newClass(getClassByName("NSException"),
                "HOCHaskellException",
                sizeof(void*),
                ivars, methods, class_methods);
        
        clsHOCHaskellException = getClassByName("HOCHaskellException");
        
        stablePtrOffset = ivars->ivar_list[0].ivar_offset;
        
        selExceptionWithNameReasonUserInfo = getSelectorForName("exceptionWithName:reason:userInfo:");
                
        excWrapperInited = YES;
    }
}

id wrapHaskellException(char *name, HsStablePtr hexc)
{
    id cexc;

#if GNUSTEP
    id (*imp)(id, SEL, id, NSString*, NSString*);
    
    initExceptionWrapper();

    imp = (void*) objc_msg_lookup(clsHOCHaskellException, selExceptionWithNameReasonUserInfo);
    
    cexc = (*imp)(clsHOCHaskellException, selExceptionWithNameReasonUserInfo,
                  utf8ToNSString("HaskellException"), utf8ToNSString(name), nil);
#else
    initExceptionWrapper();

    cexc = objc_msgSend(clsHOCHaskellException, selExceptionWithNameReasonUserInfo,
                        utf8ToNSString("HaskellException"), utf8ToNSString(name), nil);
#endif
    
    * (HsStablePtr*) (((char*)cexc) + stablePtrOffset) = hexc;
    
    
    return cexc;
}

HsStablePtr unwrapHaskellException(id cexc)
{
#if GNUSTEP
    if(cexc->class_pointer == clsHOCHaskellException)
#else
    if(cexc->isa == clsHOCHaskellException)
#endif
    {
        return *(HsStablePtr*) (((char*)cexc) + stablePtrOffset);
    }
    else
        return nil;
}