About HOC
=========

HOC is a Haskell to Objective-C binding. In a nutshell, it enables you to
use Objective-C objects and frameworks from Haskell, and also enables you
to write Objective-C objects in Haskell.

The Haskell interfaces produced by HOC are:

* Typed: Take advantage of Haskell's sound type inferenceto help you
  develop robust, correct Cocoa/GNUstep applications on your first
  compile.

* Automatically Generated: HOC comes with an interface generator to
  generate Haskell bindings to Objective-C's objects; use it even with
  your own custom Objective-C frameworks!

* Haskell-Friendly: We make heavy use of key Haskell features such as
  type classes and partial application, to ensure that the HOC  API is as
  'Haskell-like' as possible.

You can use HOC to write full-blown GUI applications using Mac OS X's
advanced Cocoa framework.

For more information on HOC, see its google code project page, at:

    http://code.google.com/p/hoc


Building HOC
============

Like many Haskell libraries, HOC ships in source form only: binaries are
not provided because of the dizzying number of configurations of GHC and
target platforms (Mac OS X 10.2, 10.3, or various Linux & GNUstep
platforms).  You'll thus have to build HOC from source code.

1. Build the HOC library and the interface generator:

  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  sudo runhaskell Setup.hs install

To run the unit tests, use:

  runhaskell Setup.hs configure -fTests
  runhaskell Setup.hs build
  ./dist/build/hoc-test/hoc-test
  sudo runhaskell Setup.hs install

2. Create the bindings:
  
  cd Bindings
  sudo HOC_SDK=MacOSX10.5 sh make-bindings-macos.sh
  cd ..
  
3. Build the hoc-wrap tool:
  
  cd Tools
  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  sudo runhaskell Setup.hs install
  cd ..  


Depending on your setup, you might be able to leae out the "sudo".
You can also add additional Cabal configure options after "configure"
commands, and after "sh make-bindings-macos.sh" (like, e.g. --user).

Authors
=======

Wolfgang Thaller <wolfgang.thaller@gmx.net>
Andre Pang <ozone@algorithm.com.au>

