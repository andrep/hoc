module TVUtilities(
        TVData(..),
        TVDataItem(..),

        SimpleTVDataSource,
        initializeClass_SimpleTVDataSource,
        setTVDataSourceData
    ) where

import HOC
import Cocoa
import AppKit.NSTableColumn(identifier)
import Prelude hiding(init)

import Data.Array

class TVData a where
    countItems :: a -> Int
    stringValueForItem :: a -> String -> Int -> IO String
    objectValueForItem :: a -> String -> Int -> IO (NSObject ())
    
    stringValueForItem listData column row = do
        object <- objectValueForItem listData column row
        object # description >>= haskellString
        
    objectValueForItem listData column row = do
        str <- stringValueForItem listData column row
        nsstr <- _NSString # stringWithHaskellString str
        return (castObject nsstr)

instance TVData () where
    countItems a = 0
    stringValueForItem = undefined
    objectValueForItem = undefined

class TVDataItem a where
    stringValueForColumn :: a -> String -> IO String
    objectValueForColumn :: a -> String -> IO (NSObject ())
    
    stringValueForColumn item column = do
        object <- objectValueForColumn item column
        object # description >>= haskellString
        
    objectValueForColumn item column = do
        str <- stringValueForColumn item column
        nsstr <- _NSString # stringWithHaskellString str
        return (castObject nsstr)

instance TVDataItem b => TVData (Array Int b) where
    countItems arr = rangeSize (Data.Array.bounds arr)

    stringValueForItem arr col row = stringValueForColumn (arr ! row) col
    objectValueForItem arr col row = objectValueForColumn (arr ! row) col
    
instance TVDataItem String where
    stringValueForColumn s _ = return s
    
data WrappedTVData = forall a. TVData a => WrappedTVData a
    
$(declareClass "SimpleTVDataSource" "NSObject")

$(exportClass "SimpleTVDataSource" "sds_" [
        InstanceVariable "theData" [t| WrappedTVData |]
                                    [| undefined |],
        
        InstanceMethod info_init,
        InstanceMethod info_numberOfRowsInTableView,
        InstanceMethod info_tableViewObjectValueForTableColumnRow
    ])

sds_init self = do
    self # setIVar _theData (WrappedTVData ())
    return self

sds_numberOfRowsInTableView tableView self = do
    WrappedTVData dat <- self # getIVar _theData
    let count = countItems dat
    return count
    
sds_tableViewObjectValueForTableColumnRow tableView column row self = do
    WrappedTVData dat <- self # getIVar _theData
    ident <- column # identifier
    identStr <- if ident == nil then return ""
                    else haskellString $ (fromID ident :: NSString ())
    objectValueForItem dat identStr row >>= return . toID

setTVDataSourceData dat self = self # setIVar _theData (WrappedTVData dat)
