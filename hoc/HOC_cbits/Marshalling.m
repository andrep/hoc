#include <Foundation/NSString.h>
#include "Marshalling.h"
#include "Selector.h"
#include "Class.h"

#ifdef GNUSTEP
#define objc_msgSend(self,sel) (*objc_msg_lookup(self,sel))(self,sel)
#define objc_msgSendWith(self,sel,args...) (*objc_msg_lookup(self,sel))(self,sel,args)
#else
#define objc_msgSendWith objc_msgSend
#endif

static SEL selAlloc = 0;
static SEL selInitWithUTF8String = 0;
static SEL selUTF8String = 0;
static Class clsNSString = 0;

const char *nsStringToUTF8(NSString *str)
{
    //return [str UTF8String];
    if(!selUTF8String)
        selUTF8String = getSelectorForName("UTF8String");
    return (const char*)objc_msgSend(str,selUTF8String);
}

NSString *utf8ToNSString(const char* cstr)
{
    //return [[NSString alloc] initWithUTF8String: cstr];
    if(!selAlloc)
        selAlloc = getSelectorForName("alloc");
    if(!selInitWithUTF8String)
        selInitWithUTF8String = getSelectorForName("initWithUTF8String:");
    if(!clsNSString)
        clsNSString = getClassByName("NSString");

    NSString *str = objc_msgSend(clsNSString,selAlloc);
    str = objc_msgSendWith(str, selInitWithUTF8String, cstr);
    return str;
}
