function build()
{
	true
}

ARGUMENTS=$*

mkdir -p Generated
cd Generated

hoc-ifgen Foundation -f -b ../binding-script.txt -a ../AdditionalCode/
build Foundation
hoc-ifgen QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation
build QuartzCore
hoc-ifgen AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore
build AppKit
hoc-ifgen CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation \
    -d AppKit -d QuartzCore     # fake dependencies
build CoreData
hoc-ifgen Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData
build Cocoa
