function build()
{
    pushd $1
    runhaskell Setup.hs configure $ARGUMENTS
    runhaskell Setup.hs build
    runhaskell Setup.hs install
    popd
}

ARGUMENTS=$*

mkdir -p Generated
cd Generated

hoc-ifgen Foundation -f -b ../binding-script.txt -a ../AdditionalCode/
build HOC-Foundation
hoc-ifgen QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation
build HOC-QuartzCore
hoc-ifgen AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore
build HOC-AppKit
hoc-ifgen CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation \
    -d AppKit -d QuartzCore     # fake dependencies
build HOC-CoreData
hoc-ifgen Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData
build HOC-Cocoa
