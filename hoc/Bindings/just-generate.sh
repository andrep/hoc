function build()
{
	true
}

ARGUMENTS=$*

IFGEN="../../InterfaceGenerator2/hoc-ifgen"

mkdir -p Generated
cd Generated

$IFGEN Foundation -f -b ../binding-script.txt -a ../AdditionalCode/
build Foundation
$IFGEN QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation
build QuartzCore
$IFGEN AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore
build AppKit
$IFGEN CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation \
    -d AppKit -d QuartzCore     # fake dependencies
build CoreData
$IFGEN Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData
build Cocoa
