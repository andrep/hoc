ARGUMENTS=$*
OPTS=-q

IFGEN=hoc-ifgen

set -e
mkdir -p Generated
cd Generated

$IFGEN Foundation -f -b ../binding-script.txt -a ../AdditionalCode/ $OPTS
$IFGEN QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation $OPTS
$IFGEN AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore $OPTS
$IFGEN CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation \
    -d AppKit -d QuartzCore $OPTS    # fake dependencies
$IFGEN Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData $OPTS
