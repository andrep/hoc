/* Area:	closure_call
   Purpose:	Check return value uchar.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

static void cls_ret_uchar_fn(ffi_cif* cif,void* resp,void** args,
			     void* userdata)
{
  *(ffi_arg*)resp = *(unsigned char *)args[0];
  printf("%d: %d\n",*(unsigned char *)args[0],
	 *(ffi_arg*)resp);
}
typedef unsigned char (*cls_ret_uchar)(unsigned char);

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  ffi_type * cl_arg_types[2];


  cl_arg_types[0] = &ffi_type_uchar;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_uchar, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_uchar_fn, NULL)  == FFI_OK);

  (*((cls_ret_uchar)pcl))(127);
  /* { dg-output "127: 127" } */

  exit(0);
}
