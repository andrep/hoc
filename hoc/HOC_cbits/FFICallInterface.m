#include "FFICallInterface.h"


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
    ffi_type *theStruct = malloc(sizeof(ffi_type));
    theStruct->type = FFI_TYPE_STRUCT;
    theStruct->size = theStruct->alignment = 0;
    theStruct->elements = elements;
    
    return theStruct;
}
