//
//  PBXCompilerSpecificationGHC.m
//  XCodeHaskellSupport
//
//  Created by André Pang on Wed Jul 30 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import "PBXCompilerSpecification.h"
#import "PBXCompilerSpecificationGHC.h"

#include <objc/objc-runtime.h>

#define LOG_CALL \
    { \
        NSLog(@"GHC: %s called", _cmd); \
    }

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


@implementation PBXCompilerSpecificationGHC : PBXCompilerSpecification

+ (void) load
{
    NSLog(@"PBXCompilerSpecificationGHC loaded.");    
}

- (id)initWithPropertyListDictionary:(id)propertyList
{
    LOG_CALL;
    
    self = [super initWithPropertyListDictionary:propertyList];
    
    return self;
}

+ (void)registerSpecification:(id)specification;
{
    LOG_CALL;

    NSLog(@"specification is %@", specification);
}

- (NSString *)uniqueOutputBaseNameForInputFilePath:(NSString *)filepath
                              inTargetBuildContext:(id)buildContext;
{
    LOG_CALL;
    
    return [[filepath stringByDeletingLastPathComponent] lastPathComponent];
}

- (NSArray *) standardFlagsInTargetBuildContext:(id)buildContxet
{
    LOG_CALL;
    
    return [[NSArray alloc] initWithObjects:@"--make"];
}

// Public API methods

- (NSString *)defaultOutputDirectory
{
    LOG_CALL;
    
    return @"/tmp";
}

- (NSString *)executablePath;
{
    LOG_CALL;
    
    return @"/usr/local/bin/ghc-6.2.1";
}

- (id)effectiveCompilerSpecificationForFileNamed:(id)filename
                                          ofType:(id)type /* PBXFileType */
                            inTargetBuildContext:(id)buildContext
{
    LOG_CALL;
    
    NSLog(@"filename:%@", filename);
    
    return [self class];
}

- (id)computeDependenciesForInputFile:(id)inputFile
                               ofType:(id)type
                              variant:(id)fp16
                         architecture:(id)fp20
                      outputDirectory:(id)fp24
                 inTargetBuildContext:(id)fp28
{
    LOG_CALL;
    
    return nil;
}

- (id)computeDependenciesForFilePath:(id)fp8
                              ofType:(id)fp12
                     outputDirectory:(id)fp16
                inTargetBuildContext:(id)fp20
{
    LOG_CALL;
    
    return nil;
}

// GCC API

- (NSString *)name
{
    LOG_CALL;
    
    return @"GHC";
}

- (NSString *)executablePathInTargetBuildContext:(id)buildContext
{
    LOG_CALL;
    
    return @"/usr/local/bin/ghc-6.2.1";
}

// - (id)defaultOutputDirectory;
// - (id)precompileHeaderFileAtPath:(id)fp8forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toPrecompPath:(id)fp20 inTargetBuildContext:(id)fp24;

- (id)compileSourceCodeFileAtPath:(id)filePath
                           ofType:(id)type
                toOutputDirectory:(id)outputDirectory
             inTargetBuildContext:(id)buildContext
{
    LOG_CALL;
    
    return [[NSArray alloc] init];
}

// - (id)computeDependenciesForInputFile:(id)fp8 ofType:(id)fp12 variant:(id)fp16 architecture:(id)fp20 outputDirectory:(id)fp24 inTargetBuildContext:(id)fp28;
// - (id)computeDependenciesForFilePath:(id)fp8 ofType:(id)fp12 outputDirectory:(id)fp16 inTargetBuildContext:(id)fp20;

- (BOOL)respondsToSelector:(SEL)aSelector
{
    NSLog (@"GHC: respondsToSelector:\"%s\" called", sel_getName(aSelector));
    return [super respondsToSelector:aSelector];
}

- (void)forwardInvocation:(NSInvocation *)anInvocation
{
    NSLOG_RECEIVING_CLASS;
    
    NSLog (@"GHC: NSInvocation received: \"%@\"", anInvocation);
    
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
    
    /* if ([shadowed_PBXCompilerSpecificationGcc respondsToSelector:
        [anInvocation selector]])
        [anInvocation invokeWithTarget:[super class]];
    else */
        [self doesNotRecognizeSelector:invocationSelector];
}

@end
