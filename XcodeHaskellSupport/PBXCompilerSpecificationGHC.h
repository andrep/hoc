//
//  PBXCompilerSpecificationGHC.h
//  XCodeHaskellSupport
//
//  Created by André Pang on Wed Jul 30 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "PBXCompilerSpecification.h"

@interface PBXCompilerSpecificationGHC : PBXCompilerSpecification {

}

#if 0

+ (Class)_specificationBaseClass;
+ (id)_localizedSpecificationTypeName;
+ (id)_specificationRegistry;
+ (id)_specificationFilenameExtension;
+ (id)_specialPropertyKeys;
+ (id)displaySpecifications;
- (id)initWithPropertyListDictionary:(id)fp8;
- (void)dealloc;
- (id)inputFileTypes;
- (BOOL)acceptsInputFileType:(id)fp8;
- (BOOL)isAbstract;
- (id)builtinJambaseRuleName;
- (id)jamfileCodeString;
- (id)effectiveCompilerSpecificationForFileNamed:(id)fp8 inTargetBuildContext:(id)fp12;
- (id)computeDependenciesForFilePaths:(id)fp8 outputDirectory:(id)fp12 inTargetBuildContext:(id)fp16;
- (id)defaultOutputDirectory;
- (id)uniqueOutputBaseNameForInputFilePath:(id)fp8 inTargetBuildContext:(id)fp12;
- (id)outputFilesForInputFilePath:(id)fp8 inTargetBuildContext:(id)fp12;

/* Public API */

- (id)executablePath;
- (id)defaultOutputDirectory;
- (id)effectiveCompilerSpecificationForFileNamed:(id)fp8 ofType:(id)fp12 inTargetBuildContext:(id)fp16;
- (id)computeDependenciesForInputFile:(id)fp8 ofType:(id)fp12 variant:(id)fp16 architecture:(id)fp20 outputDirectory:(id)fp24 inTargetBuildContext:(id)fp28;
- (id)computeDependenciesForFilePath:(id)fp8 ofType:(id)fp12 outputDirectory:(id)fp16 inTargetBuildContext:(id)fp20;

/* GCC API */
+ (id)systemGccVersionString;
- (id)name;
- (id)executablePathInTargetBuildContext:(id)fp8;
- (id)effectiveCompilerSpecificationForFileNamed:(id)fp8 ofType:(id)fp12 inTargetBuildContext:(id)fp16;
- (id)defaultOutputDirectory;
- (id)precompileHeaderFileAtPath:(id)fp8 forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toPrecompPath:(id)fp20 inTargetBuildContext:(id)fp24;
- (id)compileSourceCodeFileAtPath:(id)fp8 ofType:(id)fp12 toOutputDirectory:(id)fp16 inTargetBuildContext:(id)fp20;
- (id)fileTypeForGccLanguageDialect:(id)fp8;
- (id)computeDependenciesForInputFile:(id)fp8 ofType:(id)fp12 variant:(id)fp16 architecture:(id)fp20 outputDirectory:(id)fp24 inTargetBuildContext:(id)fp28;
- (id)computeDependenciesForFilePath:(id)fp8 ofType:(id)fp12 outputDirectory:(id)fp16 inTargetBuildContext:(id)fp20;

/* GCC 2.95.2 API */
- (id)defaultOutputDirectory;
- (id)perSpecificationFlagsInTargetBuildContext:(id)fp8;
- (id)optionalFrameworkSearchPathsInBuildContext:(id)fp8;
- (id)optionalHeaderSearchPathsInBuildContext:(id)fp8;
- (id)messageLengthFlagInTargetBuildContext:(id)fp8;
- (id)zeroLinkFlagInTargetBuildContext:(id)fp8;
- (id)fixAndContinueFlagInTargetBuildContext:(id)fp8;
- (id)dynamicNoPicFlagInTargetBuildContext:(id)fp8;
- (id)distributedBuildFlagsInTargetBuildContext:(id)fp8;
- (id)perCompilerStandardBuildFlagsInTargetBuildContext:(id)fp8;
- (id)additionalEnvironmentEntriesInTargetBuildContext:(id)fp8;
- (id)standardFlagsInTargetBuildContext:(id)fp8;
- (id)otherFlagsInTargetBuildContext:(id)fp8;
- (id)precompFileNameForHeaderPath:(id)fp8 inTargetBuildContext:(id)fp12;
- (id)flagsForIncludingPrecompiledPrefixHeaderAtPath:(id)fp8 inTargetBuildContext:(id)fp12;
- (id)flagsNotAffectingPrecompValidity;
- (id)symrepFileNameForHeaderPath:(id)fp8 inTargetBuildContext:(id)fp12;
- (id)subprocessCommandLineForPreprocessingBehaviorWithNode:(id)fp8 commandLine:(id)fp12;
- (id)precompileHeaderFileAtPath:(id)fp8 forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toPrecompPath:(id)fp20 inTargetBuildContext:(id)fp24;
- (id)symbolizeHeaderFileAtPath:(id)fp8 forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toSymbolSeparationRepositoryPath:(id)fp20 inTargetBuildContext:(id)fp24;
- (id)compileSourceCodeFileAtPath:(id)fp8 ofType:(id)fp12 toOutputDirectory:(id)fp16 inTargetBuildContext:(id)fp20;


#endif

@end
