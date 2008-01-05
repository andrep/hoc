#include <ffi.h>

#ifdef __OBJC__
@class NSException;
#else
typedef void NSException;
#endif

NSException *callWithExceptions(ffi_cif *cif, void (*fn)(),
                                void *rvalue, void **avalue);
