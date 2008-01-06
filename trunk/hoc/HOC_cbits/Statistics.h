#include <objc/objc.h>

enum
{
    kHOCAboutToEnterHaskell = 0,
    kHOCEnteredHaskell,
    kHOCImportedArguments,
    kHOCAboutToExportResult,
    kHOCAboutToLeaveHaskell,
    kHOCLeftHaskell
};
void recordHOCEvent(int what, void ** args);

