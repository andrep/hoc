module Main where

import Test.HUnit
import Prelude hiding(init)
import Foundation hiding(test)
import Foundation.NSObject(init)

import System.Mem           ( performGC )
import Control.Concurrent   ( threadDelay )
import Control.Monad        ( when )
import Control.Exception    ( try, finally )

import Selectors

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

$(declareClass "HaskellObjectWithDescription" "NSObject")

$(exportClass "HaskellObjectWithDescription" "ho2_" [
        InstanceMethod info_description
    ])
    
ho2_description self
    = do
        superDesc <- fmap fromNSString $ super self # description
        return $ toNSString $ head (words superDesc) ++ " TEST>"

$(declareClass "HaskellObjectWithIVar" "HaskellObjectWithOutlet")

$(exportClass "HaskellObjectWithIVar" "ho3_" [
        InstanceVariable "magicNumber" [t| Integer |] [| 0 |]
    ])


$(declareClass "ExceptionThrower" "NSObject")

instance Has_throwHaskellException (ExceptionThrower a)
instance Has_throwNSException (ExceptionThrower a)

$(exportClass "ExceptionThrower" "et_" [
        InstanceMethod info_throwHaskellException,
        InstanceMethod info_throwNSException
    ])

et_throwHaskellException self = fail "Test Exception"
et_throwNSException self = _NSException # exceptionWithNameReasonUserInfo
                                        (toNSString "FooBar")
                                        (toNSString "baz")
                                        nil
                            >>= raise

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
                expected <- readFile "TestFoundation.hs"
                actual_ns <- _NSString # alloc >>= initWithContentsOfFile
                                                    (toNSString "TestFoundation.hs")
                fromNSString actual_ns @?= expected
            ),
            "Unicode" ~:
                let zhongwen = "\x4E2D\x6587" -- "Chinese" in Chinese
                    fermata = "\x1D110"       -- Fermata (Musical Symbol)
                in test [
                    "nsString-haskellString" ~: (assertNoLeaks $ do
                        actual <- nsString zhongwen >>= haskellString
                        actual @?= zhongwen
                    ),
                    "length" ~: (assertNoLeaks $ do
                        actual <- nsString zhongwen >>= Foundation.length
                        actual @?= 2
                    ),
                    "nsString-haskellString-fermata" ~: (assertNoLeaks $ do
                        actual <- nsString fermata >>= haskellString
                        actual @?= fermata
                    ),
                    "length-fermata" ~: (assertNoLeaks $ do
                        actual <- nsString fermata >>= Foundation.length
                        actual @?= 2 -- yes, 2. NSString uses UTF-16.
                    )
            ]
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
        "HaskellObjectWithIVar" ~: test [
            "alloc-init" ~: (assertNoLeaks $ do
                _HaskellObjectWithIVar # alloc >>= init >> return ()
            ),
            "set-get-superclass" ~: (assertNoLeaks $ do
                num <- _NSNumber # alloc >>= initWithInt 42
                hobj <- _HaskellObjectWithIVar # alloc >>= init
                hobj # setOtherObject num
                num' <- hobj # otherObject >>= return . castObject
                when (num /= num') $ assert "Different Object returned."
            ),
            "set-get" ~: (assertNoLeaks $ do
                hobj <- _HaskellObjectWithIVar # alloc >>= init
                hobj # setIVar _magicNumber 42
                answer <- hobj # getIVar _magicNumber
                when (answer /= 42) $ assert "Different Value returned."
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
        ],
        "Super" ~: (assertNoLeaks $ do
            hobj <- _HaskellObjectWithDescription # alloc >>= init
            str <- hobj # description
            fromNSString str @?= "<HaskellObjectWithDescription: TEST>"
        ),
        "structs" ~: test [
            "pointArg" ~: (do
                let point = NSPoint 6.42 7.42
                result <- _NSValue # valueWithPoint point
                return ()
            ),
            "point" ~: (do
                let point = NSPoint 6.42 7.42
                result <- _NSValue # valueWithPoint point >>= pointValue
                result @?= point
            ),
            "size" ~: (do
                let size = NSSize 6.42 7.42
                result <- _NSValue # valueWithSize size >>= sizeValue
                result @?= size
            ),
            "rect" ~: (do
                let rect = NSRect (NSPoint 1 2) (NSSize 3 4)
                result <- _NSValue # valueWithRect rect >>= rectValue
                result @?= rect
            )
        ],
        "externConstant" ~: (
            fromNSString nsParseErrorException @?= "NSParseErrorException"
        ),
        "externFunction" ~: (do
            result <- nsStringFromPoint (NSPoint 42 23)
            fromNSString result @?= "{42, 23}"
        ),
        "exceptions" ~: test [
            "CtoH" ~: (do
                exc1 <- _NSException # exceptionWithNameReasonUserInfo
                                        (toNSString "FooBar")
                                        (toNSString "baz")
                                        nil
                result <- (exc1 # raise >> return "No Exception")
                        `catchNS` \e -> e # name >>= return . fromNSString 
                result @?= "FooBar"
            ),
            "HtoCtoH" ~: (do
                obj <- _ExceptionThrower # alloc >>= init
                result <- try (obj # throwHaskellException)
                show result @?= "Left user error (Test Exception)"
            ),
            "CtoHtoCtoH" ~: (do
                obj <- _ExceptionThrower # alloc >>= init
                result <- (obj # throwNSException >> return "No Exception")
                        `catchNS` \e -> e # name >>= return . fromNSString 
                result @?= "FooBar"
            )
        ]
    ]

go = withAutoreleasePool $ runTestTT tests

main = do
    initializeClass_HaskellObjectWithOutlet
    initializeClass_HaskellObjectWithDescription
    initializeClass_HaskellObjectWithIVar
    initializeClass_ExceptionThrower
    go
