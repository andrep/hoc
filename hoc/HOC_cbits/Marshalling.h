#ifdef __OBJC__
@class NSString;
#else
typedef void NSString;
#endif

const char *nsStringToUTF8(NSString *);
NSString *utf8ToNSString(const char*);
