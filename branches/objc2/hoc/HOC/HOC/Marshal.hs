{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}

module HOC.Marshal (ObjCMarshal, marshal) where

import HOC.Base
import Foundation.NSArray
import Foundation.NSString

import List(genericLength)

class ObjCMarshal from to where
  marshal :: from -> to

instance ObjCMarshal String (ID a) where
  marshal = castObject.toNSString

instance ObjCMarshal (ID a) String where
  marshal = fromNSString.castObject

instance ObjCMarshal (IO String) (IO (ID a)) where
  marshal stringAction = do
    string <- stringAction
    return $ castObject (toNSString string)

instance ObjCMarshal (IO [String]) (IO (NSArray ())) where
  marshal stringsAction = do
    strings <- stringsAction
    marshal strings

instance ObjCMarshal a b => ObjCMarshal [a] [b] where
  marshal = marshalList

marshalList list = map marshal list

instance ObjCMarshal a b => ObjCMarshal [a] ( IO [b] ) where
  marshal = marshalListAsIOList

marshalListAsIOList x = return (marshal x)

instance ObjCMarshal (ID a) (ID b) where
  marshal = castObject

instance ObjCMarshal a (ID b) => ObjCMarshal [a] ( IO (NSArray ()) ) where
  marshal list = do
    let marshalledList = marshalList list :: [ID b]
    newArray <- _NSMutableArray # alloc
             >>= initWithCapacity (genericLength list)
    mapM_ (\e -> addObject e newArray) marshalledList
    return (castObject newArray)

instance ObjCMarshal (NSArray ()) (IO [ID a]) where
  marshal = fromNSArray

instance ObjCMarshal (NSArray ()) (IO [String]) where
  marshal list = do
    objects <- marshal list
    return $ map fromNSString objects
  
fromNSArray array = do
  arrayLength <- array # count
  list <- sequence [ array # objectAtIndex i | i <- [0..arrayLength-1] ]
  -- XXX: replace above with an enumerator
  return $ map castObject list


