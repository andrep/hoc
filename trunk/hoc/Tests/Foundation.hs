module Main where

import HUnit
import Prelude hiding(init)
import Foundation hiding(test)
import Foundation.NSObject(init)

import System.Mem           ( performGC )
import Control.Concurrent   ( threadDelay )
import Control.Monad        ( when )
import Control.Exception    ( try, finally )


      -- garbage collect and make really sure that finalizers have time to run
performGCAndWait targetCount time maxRepeat = do
    performGC
    threadDelay time
    (objects', immortal') <- objectMapStatistics
    when (objects' - immortal' > targetCount && maxRepeat > 0) $
        performGCAndWait targetCount time (maxRepeat - 1)
    
assertLeaks leaks action = do
    (objects, immortal) <- objectMapStatistics
    let targetCount = objects - immortal + leaks
    result <- action `finally` performGCAndWait targetCount 10000 25
    
    (objects', immortal') <- objectMapStatistics
    assertEqual "Live objects after allocation,"
                targetCount (objects' - immortal')
    return result

assertNoLeaks action = assertLeaks 0 action

$(declareClass "HaskellObjectWithOutlet" "NSObject")

$(declareSelector "otherObject" [t| IO (ID ()) |])
$(declareSelector "setOtherObject:" [t| forall a. ID a -> IO () |])

instance Has_otherObject (HaskellObjectWithOutlet a)
instance Has_setOtherObject (HaskellObjectWithOutlet a)

$(exportClass "HaskellObjectWithOutlet" "ho1_" [
        Outlet "otherObject" [t| ID () |]
    ])

tests = test [
        "NSNumber" ~: test [
            "alloc-initWithInt-intValue" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithInt 42 >>= intValue
                actual @?= 42
            ),
            "numberWithInt-intValue" ~: (assertNoLeaks $ do
                actual <- _NSNumber # numberWithInt 42 >>= intValue
                actual @?= 42
            ),
            "float" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithFloat 4.2 >>= floatValue
                actual @?= 4.2
            ),
            "char" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithChar 42 >>= charValue
                actual @?= 42
            ),
            "short" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithShort 42 >>= shortValue
                actual @?= 42
            ),
            "char->int" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithChar 42 >>= intValue
                actual @?= 42
            ),
            "int->char" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithInt 42 >>= charValue
                actual @?= 42
            ),
            "bool" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithBool True >>= boolValue
                actual @?= True
                actual <- _NSNumber # alloc >>= initWithBool False >>= boolValue
                actual @?= False
            ),
            "longlong" ~: (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithLongLong 0xDEADBEEFFACED00D
                                            >>= longLongValue
                actual @?= 0xDEADBEEFFACED00D
            )
        ],
        "NSString" ~: test [
            "nsString-haskellString" ~: (assertNoLeaks $ do
                actual <- nsString "Hello, world." >>= haskellString
                actual @?= "Hello, world."
            ),
            "toNSString-fromNSString" ~: (assertNoLeaks $ do
                fromNSString (toNSString "Hello, world.") @?= "Hello, world."
            ),
            "initWithContentsOfFile" ~: (assertNoLeaks $ do
                expected <- readFile "Test1.hs"
                actual_ns <- _NSString # alloc >>= initWithContentsOfFile (toNSString "Test1.hs")
                fromNSString actual_ns @?= expected
            )
        ],
        "HaskellObjectWithOutlet" ~: test [
            "alloc-init" ~: (assertNoLeaks $ do
                _HaskellObjectWithOutlet # alloc >>= init >> return ()
            ),
            "set-get" ~: (assertNoLeaks $ do
                num <- _NSNumber # alloc >>= initWithInt 42
                hobj <- _HaskellObjectWithOutlet # alloc >>= init
                hobj # setOtherObject num
                num' <- hobj # otherObject >>= return . castObject
                when (num /= num') $ assert "Different Object returned."
            )
        ],
        "Memory" ~: test [
            "NSMutableArray-Circle" ~: (assertNoLeaks $ do
                array1 <- _NSMutableArray # alloc >>= Foundation.NSObject.init
                array2 <- _NSMutableArray # alloc >>= Foundation.NSObject.init
                array1 # addObject array2
                array2 # addObject array1
            ),
            "NSMutableArray-Circle-with-Haskell" ~: (assertLeaks 2 $ do
                hobj <- _HaskellObjectWithOutlet # alloc >>= init
                array <- _NSMutableArray # alloc >>= Foundation.NSObject.init
                array # addObject hobj
                hobj # setOtherObject array
            ),
            "HaskellObjectCircle" ~: (assertNoLeaks $ do
                hobj1 <- _HaskellObjectWithOutlet # alloc >>= init
                hobj2 <- _HaskellObjectWithOutlet # alloc >>= init
                hobj1 # setOtherObject hobj2
                hobj2 # setOtherObject hobj1
            )
        ],
        "nil" ~: test [
            "eq" ~: (do
                when (nil /= nil) $ assert "nil not equal"
            ),
            "message" ~: (do
                let nilNumber = nil :: NSNumber ()
                result <- try (nilNumber # intValue)
                expected <- try (fail "Message sent to nil: intValue")
                result @?= expected
            )            
        ]
    ]

go = withAutoreleasePool $ runTestTT tests

main = do
    initializeClass_HaskellObjectWithOutlet
    go
