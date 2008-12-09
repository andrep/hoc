#include <objc/objc.h>
#include "NewClass.h"
#include "Class.h"
#include "Ivars.h"
#include "Methods.h"
#include "Selector.h"
#include "Marshalling.h"
#include "HsFFI.h"

#define hsExceptionClassName "HOCHaskellException"
#define hsExceptionIvarName "_haskellException"

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
    super.class = getSuperClassForObject(self);
    
    (*objc_msg_lookup_super(&super, selDealloc))(self, selDealloc);
#else
    super.receiver = self;
    
#   ifdef __OBJC2__
        super.super_class = getSuperClassForObject(self);
#   else
        super.class = getSuperClassForObject(self);
#   endif

    objc_msgSendSuper(&super, selDealloc);
#endif
}

static void initExceptionWrapper()
{
    if(!excWrapperInited)
    {
        struct hoc_method_list *methods = makeMethodList(1);
        struct hoc_method_list *class_methods = makeMethodList(0);
        struct hoc_ivar_list *ivars = makeIvarList(1);
        struct objc_ivar *stablePtrIvar;
        
        selDealloc = getSelectorForName("dealloc");
        
        setMethodInListWithIMP(methods, 0, selDealloc, "v@:", (IMP) &exc_dealloc);
        
        setIvarInList(ivars, 0, hsExceptionIvarName, "^v", sizeof(void *), IVAR_PTR_ALIGN);
      
        newClass(getClassByName("NSException"),
                hsExceptionClassName,
                ivars, methods, class_methods);
        
        clsHOCHaskellException = getClassByName("HOCHaskellException");
        
        stablePtrIvar = class_getInstanceVariable(clsHOCHaskellException, hsExceptionIvarName);
#ifdef __OBJC2__
        stablePtrOffset = ivar_getOffset(stablePtrIvar);
#else
        stablePtrOffset = stablePtrIvar->ivar_offset;
#endif
        
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
