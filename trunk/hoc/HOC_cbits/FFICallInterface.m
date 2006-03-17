#include "FFICallInterface.h"
#include <stdlib.h>

ffi_cif * allocCif()
{
    return (ffi_cif*) calloc(1,sizeof(ffi_cif));
}

ffi_abi defaultABI()
{
    return FFI_DEFAULT_ABI;
}

ffi_type * allocStructType(ffi_type **elements)
{
    ffi_type *theStruct = (ffi_type *) malloc(sizeof(ffi_type));
    theStruct->type = FFI_TYPE_STRUCT;
    theStruct->size = theStruct->alignment = 0;
    theStruct->elements = elements;
    
    return theStruct;
}

int cifIsStret(ffi_cif *cif)
{
    if(cif->rtype->type == FFI_TYPE_STRUCT)
    {
#ifdef __i386__
            // on Darwin/x86, structs 8 bytes and smaller are returned
            // in registers, and we have to use objc_msgSend and not
            // objc_msgSend_stret.
        return cif->rtype->size > 8;
#else
        return 1;
#endif
    }
    else
        return 0;
}
