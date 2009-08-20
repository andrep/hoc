#import <Foundation/NSException.h>
#import <Foundation/NSString.h>
#include <ffi.h>

#include "Invocation.h"


NSException *callWithExceptions(ffi_cif *cif, void (*fn)(),
                                void *rvalue, void **avalue)
{
#ifdef GNUSTEP
    // GHC messes up the stack frame chain.
    // GNUstep generates exception stack traces on the assumption
    // that the stack frame chain makes sense.
    // Therefore, we move the frame pointer value left behind by
    // GHC from its regular stack slot into a local variable and
    // set the stack-frame link to 0 (temporarily).
    
    void **frame = __builtin_frame_address(0);
    void *save = *frame;
    
    // We assume that
    // *(void**)__builtin_frame_address(0) == __builtin_frame_address(1)
    // which is true at least for i386, x86_64 and powerpc[64].
    assert(save == __builtin_frame_address(1));

    *frame = 0;
#endif

    NSException *exception = nil;
	NS_DURING
		ffi_call(cif, fn, rvalue, avalue);
	NS_HANDLER
		exception = localException;
	NS_ENDHANDLER
	
#ifdef GNUSTEP
    // restore old stack frame link
    *frame = save;
#endif

	return exception;
}

