module BrowserController where

import Cocoa
import TVUtilities
import Data.Array
import Data.List        ( isPrefixOf, elemIndex, sort )

import Selectors

frameworks = ["Foundation", "AppKit"]

data SelInfo = SelInfo {
        haskellName :: String,
        objCName :: String,
        haskellType :: String,
        inModule :: String,
        inFramework :: Int
    } deriving(Eq, Ord)
    
instance TVDataItem SelInfo where
    stringValueForColumn si "haskellname" = return $ haskellName si
    stringValueForColumn si "objcname" = return $ objCName si
    stringValueForColumn si "type" = return $ haskellType si
    stringValueForColumn si "definedin" = return $ inModule si


$(declareClass "BrowserController" "NSObject")

$(exportClass "BrowserController" "bc_" [
        Outlet "sideBarDataSource" [t| SimpleTVDataSource () |],
        Outlet "sideBarTableView" [t| NSTableView () |],
        Outlet "selectorsDataSource" [t| SimpleTVDataSource () |],
        Outlet "selectorsTableView" [t| NSTableView () |],
        
        InstanceVariable "allSelectors" [t| [SelInfo] |] [| [] |],
        
        InstanceMethod info_awakeFromNib,
        InstanceMethod info_sideBarSelection
    ])

mkArray xs = listArray (0, Prelude.length xs - 1) xs

sideBarThings = mkArray $ "All" : frameworks

obj #. var = obj # getIVar var

bc_awakeFromNib self = do
    self #. _sideBarDataSource >>= setTVDataSourceData sideBarThings
    self #. _sideBarTableView >>= reloadData

    resourcePathAsNSString <- _NSBundle # mainBundle >>= bundlePath
    resourcePath <- haskellString resourcePathAsNSString
    let selectorsPath = resourcePath ++
                        "/Contents/Resources/all-selectors.txt"

    putStrLn $ "Loading selectors from `" ++ selectorsPath ++ "'"
    allSels <- fmap (map read . lines) $ readFile $ selectorsPath
    putStrLn $ show (Prelude.length allSels) ++ " selectors total."
    let classifiedSels = sort $ map (
            \(haskell, objc, typ, mod) ->
                let framework = case elemIndex (takeWhile (/= '.') mod) frameworks of
                                    Nothing -> 0
                                    Just x -> x + 1
                in SelInfo haskell objc typ mod framework
            ) allSels
    putStrLn $ show (Prelude.length classifiedSels) ++ " selectors sorted."
    self # setIVar _allSelectors classifiedSels

    self # bc_sideBarSelection nil
    
bc_sideBarSelection sender self = do
    allSels <- self #. _allSelectors

    row <- self #. _sideBarTableView >>= selectedRow
    putStrLn $ "sidebar selection: " ++ show row
    
    let filteredSels | row == 0 = allSels
                     | otherwise = filter (\si -> row == inFramework si)
                                          allSels
    
    putStrLn $ show (Prelude.length filteredSels) ++ " selectors displayed"
    
    self #. _selectorsDataSource
        >>= setTVDataSourceData (mkArray $ filteredSels)
    self #. _selectorsTableView >>= reloadData
