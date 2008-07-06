function build()
{
    pushd $1
    runhaskell Setup.hs configure $ARGUMENTS
    runhaskell Setup.hs build
    sudo runhaskell Setup.hs install
    popd
}

ARGUMENTS=$*

set -e
mkdir -p Generated
cd Generated

hoc-ifgen Foundation -f -b ../binding-script.txt -a ../AdditionalCode/ --headers=$GNUSTEP_SYSTEM_ROOT/Library/Headers/GNUstepBase/
build HOC-Foundation
hoc-ifgen AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d GNUstepBase --headers=$GNUSTEP_SYSTEM_ROOT/Library/Headers/GNUstepGUI/
build HOC-AppKit
hoc-ifgen Cocoa -u -d Foundation -d AppKit
build HOC-Cocoa

