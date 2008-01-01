#import <Foundation/NSException.h>
#import <Foundation/NSString.h>
#include <ffi/ffi.h>

#include "Invocation.h"


NSException *callWithExceptions(ffi_cif *cif, void (*fn)(),
                                void *rvalue, void **avalue)
{
	NS_DURING
		ffi_call(cif, fn, rvalue, avalue);
	NS_HANDLER
		return localException;
	NS_ENDHANDLER
	return nil;
}
