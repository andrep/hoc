//
//  PBXCompilerSpecificationYaccLogging.m
//  XCodeHaskellSupport
//
//  Created by André Pang on Fri May 21 2004.
//  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
//

#import "PBXCompilerSpecificationYaccLogging.h"

#include <objc/objc-runtime.h>

@implementation PBXCompilerSpecificationGcc2_95_2 (Logger)

+ (void) load
{
    NSLog(@"PBXCompilerSpecificationYaccLogging loading ...");
    
    shadowed_PBXCompilerSpecificationYacc = objc_getClass
        ("PBXCompilerSpecificationYacc");
    NSLog(@"PBXCompilerSpecificationYacc is at %p",
          shadowed_PBXCompilerSpecificationYacc);

    /* INSERT SHADOWED SELECTOR DECLARATIONS HERE */
#if 0
    shadowed_initWithPropertyListDictionary_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(initWithPropertyListDictionary:)];
    NSLog(@"initWithPropertyListDictionary: is at %p", shadowed_initWithPropertyListDictionary_);
    
    shadowed_acceptsInputFileType_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(acceptsInputFileType:)];
    NSLog(@"acceptsInputFileType: is at %p", shadowed_acceptsInputFileType_);
    
    shadowed_effectiveCompilerSpecificationForFileNamed_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(effectiveCompilerSpecificationForFileNamed:inTargetBuildContext:)];
    NSLog(@"effectiveCompilerSpecificationForFileNamed:inTargetBuildContext: is at %p", shadowed_effectiveCompilerSpecificationForFileNamed_inTargetBuildContext_);
    
    shadowed_computeDependenciesForFilePaths_outputDirectory_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(computeDependenciesForFilePaths:outputDirectory:inTargetBuildContext:)];
    NSLog(@"computeDependenciesForFilePaths:outputDirectory:inTargetBuildContext: is at %p", shadowed_computeDependenciesForFilePaths_outputDirectory_inTargetBuildContext_);
    
    shadowed_uniqueOutputBaseNameForInputFilePath_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(uniqueOutputBaseNameForInputFilePath:inTargetBuildContext:)];
    NSLog(@"uniqueOutputBaseNameForInputFilePath:inTargetBuildContext: is at %p", shadowed_uniqueOutputBaseNameForInputFilePath_inTargetBuildContext_);
    
    shadowed_outputFilesForInputFilePath_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(outputFilesForInputFilePath:inTargetBuildContext:)];
    NSLog(@"outputFilesForInputFilePath:inTargetBuildContext: is at %p", shadowed_outputFilesForInputFilePath_inTargetBuildContext_);
    
    shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(effectiveCompilerSpecificationForFileNamed:ofType:inTargetBuildContext:)];
    NSLog(@"effectiveCompilerSpecificationForFileNamed:ofType:inTargetBuildContext: is at %p", shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_);
    
    shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(computeDependenciesForInputFile:ofType:variant:architecture:outputDirectory:inTargetBuildContext:)];
    NSLog(@"computeDependenciesForInputFile:ofType:variant:architecture:outputDirectory:inTargetBuildContext: is at %p", shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_);
    
    shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_ = [PBXCompilerSpecification instanceMethodForSelector:@selector(computeDependenciesForFilePath:ofType:outputDirectory:inTargetBuildContext:)];
    NSLog(@"computeDependenciesForFilePath:ofType:outputDirectory:inTargetBuildContext: is at %p", shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_);
    
    shadowed_executablePathInTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(executablePathInTargetBuildContext:)];
    NSLog(@"executablePathInTargetBuildContext: is at %p", shadowed_executablePathInTargetBuildContext_);
    
    shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(effectiveCompilerSpecificationForFileNamed:ofType:inTargetBuildContext:)];
    NSLog(@"effectiveCompilerSpecificationForFileNamed:ofType:inTargetBuildContext: is at %p", shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_);
    
    shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(precompileHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toPrecompPath:inTargetBuildContext:)];
    NSLog(@"precompileHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toPrecompPath:inTargetBuildContext: is at %p", shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_);
    
    shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(compileSourceCodeFileAtPath:ofType:toOutputDirectory:inTargetBuildContext:)];
    NSLog(@"compileSourceCodeFileAtPath:ofType:toOutputDirectory:inTargetBuildContext: is at %p", shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_);
    
    shadowed_fileTypeForGccLanguageDialect_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(fileTypeForGccLanguageDialect:)];
    NSLog(@"fileTypeForGccLanguageDialect: is at %p", shadowed_fileTypeForGccLanguageDialect_);
    
    shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(computeDependenciesForInputFile:ofType:variant:architecture:outputDirectory:inTargetBuildContext:)];
    NSLog(@"computeDependenciesForInputFile:ofType:variant:architecture:outputDirectory:inTargetBuildContext: is at %p", shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_);
    
    shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_ = [PBXCompilerSpecificationGcc instanceMethodForSelector:@selector(computeDependenciesForFilePath:ofType:outputDirectory:inTargetBuildContext:)];
    NSLog(@"computeDependenciesForFilePath:ofType:outputDirectory:inTargetBuildContext: is at %p", shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_);
    
    shadowed_perSpecificationFlagsInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(perSpecificationFlagsInTargetBuildContext:)];
    NSLog(@"perSpecificationFlagsInTargetBuildContext: is at %p", shadowed_perSpecificationFlagsInTargetBuildContext_);
    
    shadowed_optionalFrameworkSearchPathsInBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(optionalFrameworkSearchPathsInBuildContext:)];
    NSLog(@"optionalFrameworkSearchPathsInBuildContext: is at %p", shadowed_optionalFrameworkSearchPathsInBuildContext_);
    
    shadowed_optionalHeaderSearchPathsInBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(optionalHeaderSearchPathsInBuildContext:)];
    NSLog(@"optionalHeaderSearchPathsInBuildContext: is at %p", shadowed_optionalHeaderSearchPathsInBuildContext_);
    
    shadowed_messageLengthFlagInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(messageLengthFlagInTargetBuildContext:)];
    NSLog(@"messageLengthFlagInTargetBuildContext: is at %p", shadowed_messageLengthFlagInTargetBuildContext_);
    
    shadowed_zeroLinkFlagInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(zeroLinkFlagInTargetBuildContext:)];
    NSLog(@"zeroLinkFlagInTargetBuildContext: is at %p", shadowed_zeroLinkFlagInTargetBuildContext_);
    
    shadowed_fixAndContinueFlagInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(fixAndContinueFlagInTargetBuildContext:)];
    NSLog(@"fixAndContinueFlagInTargetBuildContext: is at %p", shadowed_fixAndContinueFlagInTargetBuildContext_);
    
    shadowed_dynamicNoPicFlagInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(dynamicNoPicFlagInTargetBuildContext:)];
    NSLog(@"dynamicNoPicFlagInTargetBuildContext: is at %p", shadowed_dynamicNoPicFlagInTargetBuildContext_);
    
    shadowed_distributedBuildFlagsInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(distributedBuildFlagsInTargetBuildContext:)];
    NSLog(@"distributedBuildFlagsInTargetBuildContext: is at %p", shadowed_distributedBuildFlagsInTargetBuildContext_);
    
    shadowed_perCompilerStandardBuildFlagsInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(perCompilerStandardBuildFlagsInTargetBuildContext:)];
    NSLog(@"perCompilerStandardBuildFlagsInTargetBuildContext: is at %p", shadowed_perCompilerStandardBuildFlagsInTargetBuildContext_);
    
    shadowed_additionalEnvironmentEntriesInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(additionalEnvironmentEntriesInTargetBuildContext:)];
    NSLog(@"additionalEnvironmentEntriesInTargetBuildContext: is at %p", shadowed_additionalEnvironmentEntriesInTargetBuildContext_);
    
    shadowed_standardFlagsInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(standardFlagsInTargetBuildContext:)];
    NSLog(@"standardFlagsInTargetBuildContext: is at %p", shadowed_standardFlagsInTargetBuildContext_);
    
    shadowed_otherFlagsInTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(otherFlagsInTargetBuildContext:)];
    NSLog(@"otherFlagsInTargetBuildContext: is at %p", shadowed_otherFlagsInTargetBuildContext_);
    
    shadowed_precompFileNameForHeaderPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(precompFileNameForHeaderPath:inTargetBuildContext:)];
    NSLog(@"precompFileNameForHeaderPath:inTargetBuildContext: is at %p", shadowed_precompFileNameForHeaderPath_inTargetBuildContext_);
    
    shadowed_flagsForIncludingPrecompiledPrefixHeaderAtPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(flagsForIncludingPrecompiledPrefixHeaderAtPath:inTargetBuildContext:)];
    NSLog(@"flagsForIncludingPrecompiledPrefixHeaderAtPath:inTargetBuildContext: is at %p", shadowed_flagsForIncludingPrecompiledPrefixHeaderAtPath_inTargetBuildContext_);
    
    shadowed_symrepFileNameForHeaderPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(symrepFileNameForHeaderPath:inTargetBuildContext:)];
    NSLog(@"symrepFileNameForHeaderPath:inTargetBuildContext: is at %p", shadowed_symrepFileNameForHeaderPath_inTargetBuildContext_);
    
    shadowed_subprocessCommandLineForPreprocessingBehaviorWithNode_commandLine_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(subprocessCommandLineForPreprocessingBehaviorWithNode:commandLine:)];
    NSLog(@"subprocessCommandLineForPreprocessingBehaviorWithNode:commandLine: is at %p", shadowed_subprocessCommandLineForPreprocessingBehaviorWithNode_commandLine_);
    
    shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(precompileHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toPrecompPath:inTargetBuildContext:)];
    NSLog(@"precompileHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toPrecompPath:inTargetBuildContext: is at %p", shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_);
    
    shadowed_symbolizeHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toSymbolSeparationRepositoryPath_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(symbolizeHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toSymbolSeparationRepositoryPath:inTargetBuildContext:)];
    NSLog(@"symbolizeHeaderFileAtPath:forSourceFileOfType:withExtraFlags:toSymbolSeparationRepositoryPath:inTargetBuildContext: is at %p", shadowed_symbolizeHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toSymbolSeparationRepositoryPath_inTargetBuildContext_);
    
    shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_ = [PBXCompilerSpecificationGcc2_95_2 instanceMethodForSelector:@selector(compileSourceCodeFileAtPath:ofType:toOutputDirectory:inTargetBuildContext:)];
    NSLog(@"compileSourceCodeFileAtPath:ofType:toOutputDirectory:inTargetBuildContext: is at %p", shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_);
#else
    shadowed_initWithPropertyListDictionary_ = (IMP) 0x96fe1694;
    shadowed_acceptsInputFileType_ = (IMP) 0x96f61264;
    shadowed_effectiveCompilerSpecificationForFileNamed_inTargetBuildContext_ = (IMP) 0x96fe1690;
    shadowed_computeDependenciesForFilePaths_outputDirectory_inTargetBuildContext_ = (IMP) 0x96fe1608;
    shadowed_uniqueOutputBaseNameForInputFilePath_inTargetBuildContext_ = (IMP) 0x96fe18b8;
    shadowed_outputFilesForInputFilePath_inTargetBuildContext_ = (IMP) 0x96fe18a4;
    shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_ = (IMP) 0x96fe20e4;
    shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_ = (IMP) 0x96fe2030;
    shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_ = (IMP) 0x96fe1b1c;
    shadowed_executablePathInTargetBuildContext_ = (IMP) 0x970918b0;
    shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_ = (IMP) 0x970919c4;
    shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_ = (IMP) 0x97091c74;
    shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_ = (IMP) 0x97091c88;
    shadowed_fileTypeForGccLanguageDialect_ = (IMP) 0x97091d20;
    shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_ = (IMP) 0x97091f24;
    shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_ = (IMP) 0x970929b0;
    shadowed_perSpecificationFlagsInTargetBuildContext_ = (IMP) 0x97092b3c;
    shadowed_optionalFrameworkSearchPathsInBuildContext_ = (IMP) 0x97092c9c;
    shadowed_optionalHeaderSearchPathsInBuildContext_ = (IMP) 0x97092cb0;
    shadowed_messageLengthFlagInTargetBuildContext_ = (IMP) 0x97092cc4;
    shadowed_zeroLinkFlagInTargetBuildContext_ = (IMP) 0x97092cd8;
    shadowed_fixAndContinueFlagInTargetBuildContext_ = (IMP) 0x97092cec;
    shadowed_dynamicNoPicFlagInTargetBuildContext_ = (IMP) 0x97092d00;
    shadowed_distributedBuildFlagsInTargetBuildContext_ = (IMP) 0x97092d14;
    shadowed_perCompilerStandardBuildFlagsInTargetBuildContext_ = (IMP) 0x97092d28;
    shadowed_additionalEnvironmentEntriesInTargetBuildContext_ = (IMP) 0x97092d3c;
    shadowed_standardFlagsInTargetBuildContext_ = (IMP) 0x97092e3c;
    shadowed_otherFlagsInTargetBuildContext_ = (IMP) 0x970932d4;
    shadowed_precompFileNameForHeaderPath_inTargetBuildContext_ = (IMP) 0x970934c4;
    shadowed_flagsForIncludingPrecompiledPrefixHeaderAtPath_inTargetBuildContext_ = (IMP) 0x970934cc;
    shadowed_symrepFileNameForHeaderPath_inTargetBuildContext_ = (IMP) 0x970935fc;
    shadowed_subprocessCommandLineForPreprocessingBehaviorWithNode_commandLine_ = (IMP) 0x97093604;
    shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_ = (IMP) 0x97093734;
    shadowed_symbolizeHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toSymbolSeparationRepositoryPath_inTargetBuildContext_ = (IMP) 0x97093748;
    shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_ = (IMP) 0x9709375c;
#endif    
    
    /* END SHADOWED DECLARATIONS */
    
    NSLog(@"PBXCompilerSpecificationYaccLogging loaded.");
}


#if 1

#if 0
+ (Class) _specificationBaseClass
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  Class retval = shadowed__specificationBaseClass ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

+ (id) _localizedSpecificationTypeName
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  id retval = shadowed__localizedSpecificationTypeName ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

+ (id) _specificationRegistry
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  id retval = shadowed__specificationRegistry ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

+ (id) _specificationFilenameExtension
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  id retval = shadowed__specificationFilenameExtension ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

+ (id) _specialPropertyKeys
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  id retval = shadowed__specialPropertyKeys ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

+ (id) displaySpecifications
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  id retval = shadowed_displaySpecifications ( self, @selector(_cmd) );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

#endif

- (id) initWithPropertyListDictionary:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  initWithPropertyListDictionary: %@", fp8);
  id retval = shadowed_initWithPropertyListDictionary_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) effectiveCompilerSpecificationForFileNamed:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  effectiveCompilerSpecificationForFileNamed: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_effectiveCompilerSpecificationForFileNamed_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) computeDependenciesForFilePaths:(id)fp8 outputDirectory:(id)fp12 inTargetBuildContext:(id)fp16 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  computeDependenciesForFilePaths: %@", fp8);
  NSLog (@"  outputDirectory: %@", fp12);
  NSLog (@"  inTargetBuildContext: %@", fp16);
  id retval = shadowed_computeDependenciesForFilePaths_outputDirectory_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) uniqueOutputBaseNameForInputFilePath:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  uniqueOutputBaseNameForInputFilePath: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_uniqueOutputBaseNameForInputFilePath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) outputFilesForInputFilePath:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  outputFilesForInputFilePath: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_outputFilesForInputFilePath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) effectiveCompilerSpecificationForFileNamed:(id)fp8 ofType:(id)fp12 inTargetBuildContext:(id)fp16 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  effectiveCompilerSpecificationForFileNamed: %@", fp8);
  NSLog (@"  ofType: %@", fp12);
  NSLog (@"  inTargetBuildContext: %@", fp16);
  id retval = shadowed_effectiveCompilerSpecificationForFileNamed_ofType_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) computeDependenciesForInputFile:(id)fp8 ofType:(id)fp12 variant:(id)fp16 architecture:(id)fp20 outputDirectory:(id)fp24 inTargetBuildContext:(id)fp28 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  computeDependenciesForInputFile: %@", fp8);
  NSLog (@"  ofType: %@", fp12);
  NSLog (@"  variant: %@", fp16);
  NSLog (@"  architecture: %@", fp20);
  NSLog (@"  outputDirectory: %@", fp24);
  NSLog (@"  inTargetBuildContext: %@", fp28);
  id retval = shadowed_computeDependenciesForInputFile_ofType_variant_architecture_outputDirectory_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16, fp20, fp24, fp28 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) computeDependenciesForFilePath:(id)fp8 ofType:(id)fp12 outputDirectory:(id)fp16 inTargetBuildContext:(id)fp20 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  computeDependenciesForFilePath: %@", fp8);
  NSLog (@"  ofType: %@", fp12);
  NSLog (@"  outputDirectory: %@", fp16);
  NSLog (@"  inTargetBuildContext: %@", fp20);
  id retval = shadowed_computeDependenciesForFilePath_ofType_outputDirectory_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16, fp20 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) executablePathInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  executablePathInTargetBuildContext: %@", fp8);
  id retval = shadowed_executablePathInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) precompileHeaderFileAtPath:(id)fp8 forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toPrecompPath:(id)fp20 inTargetBuildContext:(id)fp24 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  precompileHeaderFileAtPath: %@", fp8);
  NSLog (@"  forSourceFileOfType: %@", fp12);
  NSLog (@"  withExtraFlags: %@", fp16);
  NSLog (@"  toPrecompPath: %@", fp20);
  NSLog (@"  inTargetBuildContext: %@", fp24);
  id retval = shadowed_precompileHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toPrecompPath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16, fp20, fp24 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) compileSourceCodeFileAtPath:(id)fp8 ofType:(id)fp12 toOutputDirectory:(id)fp16 inTargetBuildContext:(id)fp20 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  compileSourceCodeFileAtPath: %@", fp8);
  NSLog (@"  ofType: %@", fp12);
  NSLog (@"  toOutputDirectory: %@", fp16);
  NSLog (@"  inTargetBuildContext: %@", fp20);
  id retval = shadowed_compileSourceCodeFileAtPath_ofType_toOutputDirectory_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16, fp20 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) fileTypeForGccLanguageDialect:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  fileTypeForGccLanguageDialect: %@", fp8);
  id retval = shadowed_fileTypeForGccLanguageDialect_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) perSpecificationFlagsInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  perSpecificationFlagsInTargetBuildContext: %@", fp8);
  id retval = shadowed_perSpecificationFlagsInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) optionalFrameworkSearchPathsInBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  optionalFrameworkSearchPathsInBuildContext: %@", fp8);
  id retval = shadowed_optionalFrameworkSearchPathsInBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) optionalHeaderSearchPathsInBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  optionalHeaderSearchPathsInBuildContext: %@", fp8);
  id retval = shadowed_optionalHeaderSearchPathsInBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) messageLengthFlagInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  messageLengthFlagInTargetBuildContext: %@", fp8);
  id retval = shadowed_messageLengthFlagInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) zeroLinkFlagInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  zeroLinkFlagInTargetBuildContext: %@", fp8);
  id retval = shadowed_zeroLinkFlagInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) fixAndContinueFlagInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  fixAndContinueFlagInTargetBuildContext: %@", fp8);
  id retval = shadowed_fixAndContinueFlagInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) dynamicNoPicFlagInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  dynamicNoPicFlagInTargetBuildContext: %@", fp8);
  id retval = shadowed_dynamicNoPicFlagInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) distributedBuildFlagsInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  distributedBuildFlagsInTargetBuildContext: %@", fp8);
  id retval = shadowed_distributedBuildFlagsInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) perCompilerStandardBuildFlagsInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  perCompilerStandardBuildFlagsInTargetBuildContext: %@", fp8);
  id retval = shadowed_perCompilerStandardBuildFlagsInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) additionalEnvironmentEntriesInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  additionalEnvironmentEntriesInTargetBuildContext: %@", fp8);
  id retval = shadowed_additionalEnvironmentEntriesInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) standardFlagsInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  standardFlagsInTargetBuildContext: %@", fp8);
  id retval = shadowed_standardFlagsInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) otherFlagsInTargetBuildContext:(id)fp8 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  otherFlagsInTargetBuildContext: %@", fp8);
  id retval = shadowed_otherFlagsInTargetBuildContext_ ( self, @selector(_cmd), fp8 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) precompFileNameForHeaderPath:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  precompFileNameForHeaderPath: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_precompFileNameForHeaderPath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) flagsForIncludingPrecompiledPrefixHeaderAtPath:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  flagsForIncludingPrecompiledPrefixHeaderAtPath: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_flagsForIncludingPrecompiledPrefixHeaderAtPath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) symrepFileNameForHeaderPath:(id)fp8 inTargetBuildContext:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  symrepFileNameForHeaderPath: %@", fp8);
  NSLog (@"  inTargetBuildContext: %@", fp12);
  id retval = shadowed_symrepFileNameForHeaderPath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) subprocessCommandLineForPreprocessingBehaviorWithNode:(id)fp8 commandLine:(id)fp12 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  subprocessCommandLineForPreprocessingBehaviorWithNode: %@", fp8);
  NSLog (@"  commandLine: %@", fp12);
  id retval = shadowed_subprocessCommandLineForPreprocessingBehaviorWithNode_commandLine_ ( self, @selector(_cmd), fp8, fp12 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

- (id) symbolizeHeaderFileAtPath:(id)fp8 forSourceFileOfType:(id)fp12 withExtraFlags:(id)fp16 toSymbolSeparationRepositoryPath:(id)fp20 inTargetBuildContext:(id)fp24 
{
  NSLog (@"Logger: \"%s\" called", _cmd);
  NSLog (@"{");
  NSLog (@"  symbolizeHeaderFileAtPath: %@", fp8);
  NSLog (@"  forSourceFileOfType: %@", fp12);
  NSLog (@"  withExtraFlags: %@", fp16);
  NSLog (@"  toSymbolSeparationRepositoryPath: %@", fp20);
  NSLog (@"  inTargetBuildContext: %@", fp24);
  id retval = shadowed_symbolizeHeaderFileAtPath_forSourceFileOfType_withExtraFlags_toSymbolSeparationRepositoryPath_inTargetBuildContext_ ( self, @selector(_cmd), fp8, fp12, fp16, fp20, fp24 );
  NSLog (@"  Returns: %@", retval);
  NSLog (@"}");
  return retval;
}

#endif

@end








