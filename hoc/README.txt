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

For more information on HOC, see its homepage, at:

    http://hoc.sourceforge.net/


Building HOC
============

Like many Haskell libraries, HOC ships in source form only: binaries are
not provided because of the dizzying number of configurations of GHC and
target platforms (Mac OS X 10.2, 10.3, or various Linux & GNUstep
platforms).  You'll thus have to build HOC from source code.

(Note: If you have checked out HOC from CVS, see the BUILDING.CVS file for
build instructions instead of this.)

HOC uses GNU autoconf for its build system, so building HOC should be
a simple matter of the standard autoconf build mantra:

  ./configure
  make
  make install

If you have a problem building HOC, please check the "Requirements"
section in the docs/HOC.html file to make sure that you have all the Good
Stuff required for HOC to build properly.  If you've got everything you
need and HOC still doesn't work, this is a _bug_ -- please see the support
page at http://hoc.sourceforge.net/support.html, and contact one of the
HOC developers to help you out.  Chances are that a few other people have
run into the same problems as you!

HOC should automatically locate where GHC is on your system, but if you
want to specify what command it should use to run GHC, HOC's ./configure
script takes a few extra flags you'll be interested in:

  --with-ghc=...          command to run ghc (e.g. $HOME/bin/ghc-6.4)
  --with-ghc-pkg=...      command to run ghc-pkg (e.g. $HOME/bin/ghc-pkg-6.4)

Note that HOC will install itself to GHC's library directory (which you
can find out yourself with `ghc --print-libdir`), so you'll need the
appropriate permissions to write to there during the 'make install'.


Authors
=======

Wolfgang Thaller <wolfgang.thaller@gmx.net>
Andre Pang <ozone@algorithm.com.au>

