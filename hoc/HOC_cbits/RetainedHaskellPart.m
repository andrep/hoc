#include <assert.h>
#include <stdlib.h>
#include <objc/objc-class.h>
#include "RetainedHaskellPart.h"

void* getRetainedHaskellPart(id obj)
{
    void *haskellPart = NULL;
    Ivar ivar = object_getInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            &haskellPart );
    assert(ivar);
    return haskellPart;
}

void setRetainedHaskellPart(id obj, void* haskellPart)
{
    Ivar ivar = object_setInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            haskellPart );
    assert(ivar);
}
