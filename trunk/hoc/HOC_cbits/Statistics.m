#import "Statistics.h"
#include <stdint.h>
#include <stdio.h>

//#define DO_TIMINGS

#ifdef DO_TIMINGS

#if !GNUSTEP
#include <mach/mach.h>
#include <mach/mach_time.h>

inline uint64_t abstime()
    { return mach_absolute_time(); }

static double tonano(uint64_t x)
{
    uint64_t time = mach_absolute_time();
    static mach_timebase_info_data_t    sTimebaseInfo;
    if ( sTimebaseInfo.denom == 0 ) {
        mach_timebase_info(&sTimebaseInfo);
    }
    return (double)x * sTimebaseInfo.numer / sTimebaseInfo.denom;
}
#endif

#endif

static double enteringTime = 0;
static double importTime = 0;
const double weight = 0.01;

void recordHOCEvent(int what, void ** args)
{
    id obj;
    SEL sel;
    obj = *(id*)  args[0];
    sel = *(SEL*) args[1];
    
   // printf("recordHOCEvent %d\n", what);
    
#ifdef DO_TIMINGS
    static uint64_t saved;
    double time;
    switch(what)
    {
        case kHOCAboutToEnterHaskell:
            saved = abstime();
            break;
        case kHOCEnteredHaskell: 
            time = tonano(abstime() - saved);
//            if(time > 100000)
//                printf("Took a long time to enter: %g\n", time);
            if(enteringTime != 0)
                enteringTime = (1-weight) * enteringTime + weight * time;
            else
                enteringTime = time;
            saved = abstime();
            break;
        case kHOCImportedArguments: 
            time = tonano(abstime() - saved);
//            if(time > 100000)
//                printf("Took a long time to import: %g\n", time);
            if(importTime != 0)
                importTime = (1-weight) * importTime + weight * time;
            else
                importTime = time;
            break;
    }
#endif
}
