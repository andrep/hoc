#include "FFICallInterface.h"


ffi_cif * allocCif()
{
    return (ffi_cif*) calloc(1,sizeof(ffi_cif));
}

ffi_abi defaultABI()
{
    return FFI_DEFAULT_ABI;
}
