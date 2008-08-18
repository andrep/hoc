#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.


#
# Basic autoconf setup
#

AC_PREREQ(2.57)
AC_INIT(HOC, 0.8, ozone@algorithm.com.au)
AC_CONFIG_AUX_DIR(autotools)

# Check we're in the right directory
AC_CONFIG_SRCDIR([HOC/HOC/NewClass.hs])

# Check system we're running on
AC_CANONICAL_SYSTEM

# Checks for Objective-C support
AC_LANG_OBJC

# Checks for programs.
AC_PROG_OBJC
AC_PROG_MAKE_SET
AC_PROG_RANLIB
# other programs to check for: ar cp ghc-pkg ld rm xargs gnumake

# Programs required for building docs
build_docs_ok=no
AC_CHECK_PROG(POD2HTML, pod2html, pod2html)
APP_GRAPHVIZ_PATH='/Applications/Graphviz.app/Contents/MacOS'
AC_PATH_PROG(DOT, "dot", no, "$APP_GRAPHVIZ_PATH:$PATH")
if test "x$POD2HTML" = "xpod2html"; then
  if test "x$DOT" != "xno"; then
    build_docs_ok=yes
  fi
fi


#
# Test for GHC: this handy autoconf macro blatantly stolen from HDoc:
# http://www.fmi.uni-passau.de/~groessli/hdoc/
#

AC_ARG_WITH(ghc,
  [  --with-ghc=...          command to run ghc (e.g. $HOME/bin/ghc-6.4)],
  [GHC=$withval])

if test -z "$GHC"; then
  AC_CHECK_TOOL(GHC, ghc, ghc)
fi

GHC_FLAGS=""

if test "$GHC"; then
  AC_MSG_CHECKING([for ghc version])
  changequote(<<, >>)
  ghc_version=`$GHC --version 2>&1 | sed -n -e 's/.* version \([0-9.][0-9.]*\).*/\1/p'`
  ghc_major=`echo "$ghc_version" | sed -n -e 's/\([0-9][0-9]*\).*/\1/p'`
  ghc_minor=`echo "$ghc_version" | sed -n -e 's/[0-9][0-9]*\.\([0-9][0-9]*\).*/\1/p'`
  changequote([, ])
  AC_MSG_RESULT([$ghc_version])
  if test '(' "$ghc_major" -ge 6 -a "$ghc_minor" -ge 4 ')' -o \
          '(' "$ghc_major" -gt 6 ')'; then
    AC_MSG_CHECKING([for ghc library directory])
    GHC_LIB_PATH=`$GHC --print-libdir`
    AC_MSG_RESULT([$GHC_LIB_PATH])
  else
    AC_MSG_ERROR([** Sorry, your GHC is too old. GHC >= 6.4 required.])
    GHC=""
  fi
else
  AC_MSG_ERROR([** Could not find GHC.])
fi

AC_SUBST(GHC)
AC_SUBST(GHC_LIB_PATH)

# Check for ghc-pkg
AC_ARG_WITH(ghc-pkg,
  [  --with-ghc-pkg=...      command to run ghc-pkg (e.g. $HOME/bin/ghc-pkg-6.4)],
  [GHC_PKG="$withval"])

if test -n "$GHC_PKG"; then
  AC_SUBST(GHC_PKG)
  true
else
  AC_CHECK_TOOL(GHC_PKG, ghc-pkg, ghc-pkg)
fi


#
# Check for various C requirements
#

AC_CHECK_HEADERS([assert.h objc/objc-api.h objc/objc-class.h objc/objc-runtime.h stdlib.h])
AC_C_CONST
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memset strdup])

#check to see if we're building a dynanic haskell lib
AC_ARG_WITH([dynamic-haskell],
            [AS_HELP_STRING([--with-dynamic-haskell],
                [whether or not to build dynamic libraries for the haskell modules.  Default is static])],
            [HocBuildDylibs=YES],
            [HocBuildDylibs=NO])
AC_SUBST(HocBuildDylibs)

#
# Determine which ffi to use
#
AC_ARG_WITH([ffi],
             [AS_HELP_STRING([--with-ffi],
               [where to find ffi.  Use either this or --with-ffi-sources @<:@default=check@:>@])],
             [],
             [with_ffi=check])
AC_ARG_WITH([ffi-sources],
             [AS_HELP_STRING([--with-ffi-sources],
               [where to find ffi sources.  Use either this or --with-ffi @<:@default=check@:>@])],
             [],
             [with_ffi_sources=check])


if test "x$with_ffi" = "xyes"; then
    with_ffi=check
fi
if test "x$with_ffi_sources" = "xyes";then
    with_ffi_sources=check
fi

if test "x$with_ffi" != "xcheck" -a "x$with_ffi_sources" != "xcheck"; then
    AC_MSG_ERROR([*** Cannot specify both --with-ffi and --with-ffi-sources.])
fi

if test "x$with_ffi_sources" = "xcheck" -a "x$with_ffi" != "xcheck"; then
    with_ffi_sources=no
fi

#
# Check for libffi in the libffi-src/ directory
#
case "$with_ffi_sources" in
    'no')
        ;;
    'check')
        AC_CHECK_FILE([libffi-src/configure], [with_ffi_sources=$(pwd)/libffi-src; $with_ffi_sources/configure], [with_ffi_sources=no])
        ;;
    /*)
        AC_CHECK_FILE([$with_ffi_sources/configure],[$with_ffi_sources/configure],[with_ffi_sources=fail])
        ;;
    *)
        with_ffi_sources=$(pwd)/$(dirname $with_ffi_sources)/$(basename $with_ffi_sources)
        AC_CHECK_FILE([$with_ffi_sources/configure],[$with_ffi_sources/configure],[with_ffi_sources=fail])
        ;;
esac

if test "x$with_ffi_sources" = "xfail"; then
    AC_MSG_ERROR([*** Unable to locate ffi-sources.  Please specify a valid ffi path with --with-ffi-sources])
elif test "x$with_ffi_sources" != "xno"; then
    with_ffi=no
fi

if test "x$with_ffi_sources" = "xno"; then
    case "$with_ffi" in
        'no')
            AC_MSG_ERROR([*** Must specify either a valid --with-ffi or a valid --with-ffi-sources])
            ;;
        'check')
            AC_CHECK_FILE([/usr/include/ffi/ffi.h], [with_ffi=/usr], [with_ffi=no])
            ;;
        /*)
            AC_CHECK_FILE([$with_ffi/include/ffi/ffi.h], [], [with_ffi=fail])
            ;;
        *)
            with_ffi=$(pwd)/$(dirname $with_ffi)/$(basename $with_ffi)
            AC_CHECK_FILE([$with_ffi/include/ffi/ffi.h], [], [with_ffi=fail])
            ;;
    esac

    if test "x$with_ffi" = "xfail"; then
        AC_MSG_ERROR([*** Unable to locate ffi.  Please specify a valid ffi path with --with-ffi])
    fi

fi

if test "x$with_ffi" = "xno" -a "x$with_ffi_sources" = "xno"; then
    AC_MSG_ERROR([*** Must specify one of --with-ffi or --with-ffi-sources])
fi

if test "x$with_ffi" != "xno"; then
    FFI_CFLAGS="-I$with_ffi/include/ffi"
    FFI_SOURCES=
    if test "x$with_ffi" == "x/usr"; then
        FFI_LIBS="-lffi"
        FFI_LIB_DIRS=
    else
        FFI_LIBS="-L$with_ffi/lib -lffi"
        FFI_LIB_DIRS="\"\\\"$with_ffi/lib\\\"\""
    fi
    PKG_EXTRA_LIBS="\", \\\"ffi\\\"\""
elif test "x$with_ffi_sources" != "xno" ; then
    FFI_CFLAGS="-I$with_ffi_sources/include"
    FFI_SOURCES=" \\
		$with_ffi_sources/src/types.c \\
		$with_ffi_sources/src/prep_cif.c \\
		$with_ffi_sources/src/x86/ffi_darwin.c \\
		$with_ffi_sources/src/x86/darwin.S \\
		$with_ffi_sources/src/powerpc/ffi_darwin.c \\
		$with_ffi_sources/src/powerpc/darwin.S \\ 
		$with_ffi_sources/src/powerpc/darwin_closure.S"
    FFI_SOURCE_DIR=$with_ffi_sources
    FFI_LIBS=
fi

AC_SUBST(FFI_CFLAGS)
AC_SUBST(FFI_SOURCES)
AC_SUBST(FFI_LIBS)
AC_SUBST(FFI_SOURCE_DIR)
AC_SUBST(FFI_LIB_DIRS)
AC_SUBST(PKG_EXTRA_LIBS)

#
# Check for Cocoa (Mac OS X) or GNUstep platform
#

AC_MSG_CHECKING([for Objective-C platform])
case "$target_os" in
  darwin*)
    AC_MSG_RESULT([Cocoa])
    platform=macos;;
  *)
    AC_MSG_RESULT([GNUstep])
    platform=gnustep;;
esac

# Platform-specific definitions go here
if test "x$platform" = "xgnustep"; then
  HOC_LD_ALL_LOAD='--whole-archive'
  HOC_MAKE_STATIC_LIB='ar cqs'
  HOC_FOUNDATION_INCLUDES='-I/usr/lib/GNUstep/System/Library/Headers/'
  HOC_FOUNDATION_LIBS='-L/usr/lib/GNUstep/System/Library/Libraries \\\
                       -L/usr/lib/gcc-lib/i486-linux/3.3.5 \\\
                       -lobjc -lgnustep-base'
  HOC_DEFINES='-DGNUSTEP'
  HOC_CFLAGS='$(FOUNDATION_INCLUDES) $(DEFINES) \\\
              -fconstant-string-class=NSConstantString \\\
              -Wno-import'

  HOC_FOUNDATION_LIB_DIRS="\"\\\"/usr/lib/GNUstep/System/Library/Libraries\\\" \\\"/usr/lib/gcc-lib/i486-linux/3.3.5\\\"\""

else
  HOC_LD_ALL_LOAD='-all_load'
  HOC_MAKE_STATIC_LIB='libtool -static -o'
  HOC_FOUNDATION_INCLUDES=''
  HOC_FOUNDATION_LIBS='-framework Foundation'
  HOC_DEFINES=''
  HOC_CFLAGS='$(FOUNDATION_INCLUDES) $(DEFINES)'
  HOC_FOUNDATION_LIB_DIRS=''
fi
HOC_PLATFORM="$platform"
AC_SUBST(HOC_PLATFORM)
AC_SUBST(HOC_LD_ALL_LOAD)
AC_SUBST(HOC_MAKE_STATIC_LIB)
AC_SUBST(HOC_FOUNDATION_INCLUDES)
AC_SUBST(HOC_FOUNDATION_LIBS)
AC_SUBST(HOC_DEFINES)
AC_SUBST(HOC_CFLAGS)
AC_SUBST(HOC_FOUNDATION_LIB_DIRS)

#
# Output configuration files and Makefiles
#

AC_SUBST(PACKAGE_VERSION)

AC_CONFIG_FILES([config.mk
                 Makefile
                 AppKit/Makefile
                 Bindings/Makefile
                 docs/Makefile
                 Foundation/Makefile
                 HOC/Makefile
                 HOC_cbits/Makefile
                 InterfaceGenerator/Makefile
                 InterfaceGenerator2/Makefile
                 Tools/Makefile
                ])

AC_OUTPUT

# Modeline for vi(m) - vi:expandtab

