#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.


#
# Basic autoconf setup
#

AC_PREREQ(2.57)
AC_INIT(HOC, 0.2, ozone@algorithm.com.au)
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
  [  --with-ghc=...          command to run ghc (e.g. $HOME/bin/ghc-6.2.1)],
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
  if test '(' "$ghc_major" -ge 6 -a "$ghc_minor" -ge 2 ')' -o \
          '(' "$ghc_major" -ge 6 ')'; then
    AC_MSG_CHECKING([for ghc library directory])
    GHC_LIB_PATH=`$GHC --print-libdir`
    AC_MSG_RESULT([$GHC_LIB_PATH])
  else
    AC_MSG_ERROR([** Sorry, your GHC is too old. GHC >= 6.2 required.])
    GHC=""
  fi
else
  AC_MSG_ERROR([** Could not find GHC.])
fi

AC_SUBST(GHC)
AC_SUBST(GHC_LIB_PATH)

# Check for ghc-pkg
AC_ARG_WITH(ghc-pkg,
  [  --with-ghc-pkg=...      command to run ghc-pkg (e.g. $HOME/bin/ghc-pkg-6.2.1)],
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



#
# Check for libffi in the libffi-src/ directory
#

AC_CHECK_FILE(libffi-src/configure, have_libffi_src=yes, have_libffi_src=no)
if test "x$have_libffi_src" = "xno"; then
  AC_MSG_ERROR([
** You need to put the libffi source distribution in the libffi-src/
** directory.  e.g.
**
**   cvs -d :pserver:anonymous@cvs.sf.net:/cvsroot/hoc co -d libffi-src libffi
**
** See the BUILDING file for more details!
])
fi


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
  HOC_FOUNDATION_INCLUDES='-I/usr/GNUstep/System/Library/Headers/'
  HOC_FOUNDATION_LIBS='-L/usr/GNUstep/System/Library/Libraries \
                       -L/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3 \
                       -lobjc -lgnustep-base'
  HOC_DEFINES='-DGNUSTEP'
  HOC_CFLAGS='$(FOUNDATION_INCLUDES) $(DEFINES) \
              -fconstant-string-class=NSConstantString \
              -Wno-import'
else
  HOC_LD_ALL_LOAD='-all_load'
  HOC_MAKE_STATIC_LIB='libtool -static -o'
  HOC_FOUNDATION_INCLUDES=''
  HOC_FOUNDATION_LIBS='-framework Foundation'
  HOC_DEFINES=''
  HOC_CFLAGS='$(FOUNDATION_INCLUDES) $(DEFINES)'
fi
HOC_PLATFORM="$platform"
AC_SUBST(HOC_PLATFORM)
AC_SUBST(HOC_LD_ALL_LOAD)
AC_SUBST(HOC_MAKE_STATIC_LIB)
AC_SUBST(HOC_FOUNDATION_INCLUDES)
AC_SUBST(HOC_FOUNDATION_LIBS)
AC_SUBST(HOC_DEFINES)
AC_SUBST(HOC_CFLAGS)


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
                 Samples/Browser/Makefile
                 Samples/Editor/Makefile
                 Samples/UniqSort/Makefile
                 Tools/Makefile
                ])

AC_OUTPUT


#
# Configure libffi-src/
#

(cd libffi-src;
 mkdir -p build && cd build;
 if ! test -f config.status; then
   echo "configuring in libffi-src...";
   ../configure --disable-shared --enable-static;
 fi
)


# Modeline for vi(m) - vi:expandtab

