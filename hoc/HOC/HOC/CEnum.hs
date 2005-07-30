module HOC.CEnum(CEnum(fromCEnum, toCEnum), declareCEnum, declareAnonymousCEnum)
    where

import HOC.Arguments    ( ObjCArgument(..) )
import HOC.TH
import HOC.NameCaseChange

import Foreign.C        ( CInt )

class CEnum a where
    fromCEnum :: a -> CInt
    toCEnum :: CInt -> a
    
declareCEnum name assocs
    = sequence $ [
            dataD (cxt []) typ []
                [ normalC n [] | n <- constructors ] [''Eq, ''Ord, ''Read, ''Show],
            instanceD (cxt []) (conT ''CEnum `appT` conT typ)
                {-  -- this causes a strange problem that
                    -- I didn't manage to reproduce yet.
                    `whereQ` [d|
                    fromCEnum = $(mkCaseMap $ zip (map (flip conP []) constructors)
                                              (map (litE . integerL) values))
                    toCEnum = $(mkCaseMap $   zip (map (litP . integerL) values)
                                              (map conE constructors)) 
                |],-}
                [
                    valD (varP 'fromCEnum) (normalB $
                        mkCaseMap $ zip (map (flip conP []) constructors)
                                    (map (litE . integerL) values)) [],
                    valD (varP 'toCEnum) (normalB $
                        mkCaseMap $ zip (map (litP . integerL) values)
                                    (map conE constructors)) []
                ],
            instanceD (cxt []) (conT ''ObjCArgument `appT` conT typ `appT` [t| CInt |])
                `whereQ` [d|
                    exportArgument = return . fromCEnum
                    importArgument = return . toCEnum
                    objCTypeString _ = "i"
                |]
        ] ++ [
            valD (varP constant) (normalB $ conE constructor) []
            | (constant, constructor) <- zip constants constructors
        ]
    where
        typ = mkName $ nameToUppercase name
        constructors = map (mkName . nameToUppercase . fst) assocs
        constants = map (mkName . nameToLowercase . fst) assocs
        values = map snd assocs
        
declareAnonymousCEnum assocs
    = sequence [
            valD (varP constant) (normalB $ litE $ integerL value) []
            | (constant, value) <- zip constants values
        ]
    where
        constants = map (mkName . nameToLowercase . fst) assocs
        values = map snd assocs

mkCaseMap ps = [| \x -> $(caseE [|x|] $ map (\(a,b) -> match a (normalB b) []) ps) |]

    -- use Read and Show classes to avoid a GHC 6.4 bug:
bug_1246483_workaround = read (show (42 :: Int)) :: Int
