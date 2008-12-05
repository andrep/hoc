module TestPreprocessor where

import Preprocessor

import Test.HUnit
import Data.Char

rstrip = reverse . dropWhile isSpace . reverse

a ==> b = rstrip (preprocess "test" a) ~?= rstrip b

success a = (filter interesting $ lines $ preprocess "test" a) ~?= ["success"]
    where
        interesting ('/' : '/' : _ ) = False
        interesting other | all isSpace other = False
                          | otherwise = True

tests = "TestPreprocessor" ~: test [
        "empty" ~: "" ==> "",
        
        "plainLines" ~: 
            let txt = "asfljkaslf\nasjfhaslkhf\naskfhaskjf\n"
            in txt ==> txt,
            
        "comment1" ~:
            "/* abc */\ndef\n/*ghi*/\n" ==> "/* abc */\ndef\n/*ghi*/\n",
        "comment2" ~:
            "/* abc\ndef */\nghi\n" ==> "/* abc*/\n/*def */\nghi\n",
            
        "ifthenelse1" ~:
            "#include <foo>\n\
            \blah\n\
            \foo bar\n\
            \#if 42\n\
            \baz\n\
            \#else\n\
            \quux\n\
            \#endif\n"
            ==> 
            "//#include <foo>\n\
            \blah\n\
            \foo bar\n\
            \//#if 1\n\
            \baz\n\
            \//#else\n\
            \//T quux\n\
            \//#endif",
            
        "elif1" ~: success
            "#if 1\n\
            \success\n\
            \#elif 1\n\
            \failure2\n\
            \#else\n\
            \failure3\n\
            \#endif",
            
        "elif2" ~: success
            "#if 0\n\
            \failure1\n\
            \#elif 1\n\
            \success\n\
            \#else\n\
            \failure2\n\
            \#endif",

        "elif3" ~: success
            "#if 0\n\
            \failure1\n\
            \#elif 0\n\
            \failure2\n\
            \#else\n\
            \success\n\
            \#endif",
        
        "elif4" ~: success
            "#if 6 * 9 == 42\n\
            \No, that is not the question.\n\
            \#elif 2 + 2 == 5\n\
            \We love Big Brother!\n\
            \#else\n\
            \success\n\
            \#endif",
            
        "elif5" ~: success
            "#if 6 * 7 == 42\n\
            \success\n\
            \#elif 2 + 2 == 5\n\
            \We love Big Brother!\n\
            \#else\n\
            \wrong, too.\n\
            \#endif",

        "elif6" ~: success
            "#if 6 * 9 == 42\n\
            \no.\n\
            \#elif MAC_OS_X_VERSION_10_5 == 1050\n\
            \success\n\
            \#else\n\
            \wrong, too.\n\
            \#endif",
            
        "nest1" ~: success
            "#if 1\n\
            \#if 1\n\
            \success\n\
            \#else\n\
            \failure1\n\
            \#endif\n\
            \#else\n\
            \failure2\n\
            \#if 1\n\
            \failure3\n\
            \#else\n\
            \failure4\n\
            \#endif\n\
            \failure5\n\
            \#endif",

        "nest2" ~: success
            "#if 0\n\
            \failure0\n\
            \#if 1\n\
            \failure1\n\
            \#else\n\
            \failure2\n\
            \#endif\n\
            \failure3\n\
            \#else\n\
            \#if 0\n\
            \failure4\n\
            \#else\n\
            \success\n\
            \#endif\n\
            \#endif",
            
        "defineBackslash" ~: success
            "#define FOO bar\\\n\
            \            baz\n\
            \success"
    ]
