//
//  PBXCompilerSpecificationGccLogging.m
//  XCodeHaskellSupport
//
//  Created by André Pang on Fri May 21 2004.
//  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
//

#import "PBXCompilerSpecificationGccLogging.h"

#include <objc/objc-runtime.h>

/*
 * Debugging macros
 */

#define DEBUG

/* Given a function name, prints out what the receiving class is for
* this function */
#define SELF_CLASSNAME (((struct objc_class *) [self class])->name)

#ifdef DEBUG
#define NSLOG_RECEIVING_CLASS \
NSLog (@"PBXCompilerSpecificationGccLogging: [%s %s] invoked", SELF_CLASSNAME, sel_getName(_cmd));
#else
#define NSLOG_RECEIVING_CLASS do {} while (0);
#endif

@implementation PBXCompilerSpecificationGcc3_3 (Logger)

+ (void) load
{
    NSLog(@"PBXCompilerSpecificationGcc logger loading ...");
	
	shadowed_PBXCompilerSpecificationGcc = objc_getClass
	    ("PBXCompilerSpecificationGcc");
        
        /* Shadowed declarations */
        shadowed_c_identifier =
            [[self class] methodForSelector:@selector(identifier:)];
#if 0
        shadowed_i_identifier =
            [self instanceMethodForSelector:@selector(identifier:)];
#else
        shadowed_i_identifier = (IMP) 0x90836760;
        NSLog(@"    shadowed_i_identifier = (IMP) %p", shadowed_i_identifier);
#endif
        NSLog(@"    shadowed_c_identifier = (IMP) %p", shadowed_c_identifier);

	NSLog(@"PBXCompilerSpecificationGccLogging loaded.");
}

+ (id) identifier
{
    id retval = shadowed_i_identifier(self, _cmd);
    NSLog (@"Logger: identifier returning %@", retval);
    return retval;
}

#if 0
- (void)forwardInvocation:(NSInvocation *)anInvocation
{
    NSLOG_RECEIVING_CLASS;
    
    NSLog (@"NSInvocation received: \"%@\"", anInvocation);
	
    id invocationTarget;
    SEL invocationSelector;
	
    // Method signature?
    int args = [[anInvocation methodSignature] numberOfArguments];
    NSLog (@"Invocation number of arguments is %d", args);
	
    [anInvocation getArgument:&invocationTarget   atIndex:0];
    [anInvocation getArgument:&invocationSelector atIndex:1];
	
    NSLog (@"Invocation details: target:%p selector:\"%s\"",
           invocationTarget, sel_getName (invocationSelector));
	
    struct objc_class *invocationTargetClass = [invocationTarget class];
    NSLog (@"Invocation target class: \"%s\"", invocationTargetClass->name);
	
	if ([shadowed_PBXCompilerSpecificationGcc respondsToSelector:
		[anInvocation selector]])
		[anInvocation invokeWithTarget:[super class]];
	else
		[self doesNotRecognizeSelector:invocationSelector];
}

- (BOOL)respondsToSelector:(SEL)aSelector
{
    NSLog (@"respondsToSelector:\"%s\" called", sel_getName(aSelector));
	return [super respondsToSelector:aSelector];
}

- (char)acceptsInputFileType:fp8;
{
	NSLog(@"PBXCompilerSpecificationGccLogging: acceptsInputFileType called");
	return [super acceptsInputFileType:fp8];
}

- (BOOL)willForwardSelector:(SEL)selector
{
    NSLog (@"HSProxy: willForwardSelector for \"%s\" called",
           sel_getName(selector));
	
    return YES;
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector
{
    const char *selectorCString = sel_getName(aSelector);
	
    NSLog (@"methodSignatureForSelector:\"%s\" called", selectorCString);
	
	return [super methodSignatureForSelector:aSelector];
}

- (char)isAbstract;
{
	NSLog(@"isAbstract called");
	return [super isAbstract];
}
#endif


@end
