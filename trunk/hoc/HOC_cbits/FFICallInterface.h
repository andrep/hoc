#include <ffi/ffi.h>

ffi_cif * allocCif();
ffi_abi defaultABI();
ffi_type * allocStructType(ffi_type **elements);

int cifIsStret(ffi_cif *cif);
