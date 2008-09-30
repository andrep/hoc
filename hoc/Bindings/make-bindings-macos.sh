function build()
{
    pushd $1
    runhaskell Setup.hs configure $ARGUMENTS
    runhaskell Setup.hs build
    runhaskell Setup.hs install
    popd
}

ARGUMENTS=$*
OPTS=

IFGEN=hoc-ifgen

set -e
mkdir -p Generated
cd Generated

$IFGEN Foundation -f -b ../binding-script.txt -a ../AdditionalCode/ $OPTS
build Foundation
$IFGEN QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation $OPTS
build QuartzCore
$IFGEN AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore $OPTS
build AppKit
$IFGEN CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation \
    -d AppKit -d QuartzCore $OPTS    # fake dependencies
build CoreData
$IFGEN Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData $OPTS
build Cocoa
