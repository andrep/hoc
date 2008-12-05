{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Entities where

import Control.Monad.State
import Data.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import Headers  ( ModuleName )
import CTypeToHaskell
import SyntaxTree ( CType, Selector )
import Progress
import SrcPos( SrcPos )

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS

type Framework = ByteString
data EntityID
    = LocalEntity Int
    | FrameworkEntity Framework Int
    | DelayedClassLookup ByteString
    | DelayedProtocolLookup ByteString
    deriving ( Read, Show, Eq, Ord, Typeable, Data )

data Module
    = LocalModule ModuleName
    | FrameworkModule Framework ModuleName
    deriving ( Read, Show, Eq, Ord, Typeable, Data )

data HaskellType c h
    = ConvertedType h [EntityID]
    | UnconvertedType c
    deriving ( Read, Show, Eq, Ord, Typeable, Data )

type HaskellValueType = HaskellType CType HType
type HaskellSelectorType = HaskellType (SelectorKind, Selector) HSelectorType

data EntityInfo
    = ClassEntity (Maybe EntityID)
    | TypeSynonymEntity HaskellValueType
    | EnumEntity Bool [(ByteString, Integer)]
    | AdditionalTypeEntity
    | SelectorEntity HaskellSelectorType
    | ProtocolEntity [EntityID] {- superprotocols -}
                     [EntityID] {- selectors -}
    | MethodEntity
    | ProtocolAdoptionEntity

    | ExternVarEntity HaskellValueType
    | ExternFunEntity HaskellSelectorType

    | ReexportEntity EntityID
    
    | AdditionalCodeEntity Int {- position -}
                           [ByteString] {- exports -}
                           ByteString {- import statements -}
                           ByteString {- text -}

    | StructEntity (Maybe String) [HaskellValueType]

    deriving ( Read, Show, Eq, Ord, Typeable, Data )
    
data Name
    = CName ByteString -- classes, typedefs, enums, variables, functions
    | ProtocolName ByteString
    | SelectorName ByteString
    
        -- anonymous in Haskell, but can be identified
    | ProtocolAdoptionName EntityID EntityID
    | SelectorInstanceName EntityID EntityID Bool {- True = factory method -}
        -- selectors are always defined in objective C, so we have an EntityID,
        -- we don't want to resolve this later by name

    | Anonymous
    deriving ( Read, Show, Eq, Ord, Typeable, Data )

data Entity = Entity {
        eName :: Name,
        eHaskellName :: ByteString,
        eAlternateHaskellNames :: [ByteString],
        eInfo :: EntityInfo,
        eModule :: Module,
        eSrcPos :: SrcPos
    }
    deriving ( Read, Show, Typeable, Data )

type EntityMap = Map.Map EntityID Entity

data EntityPile = EntityPile {
        epEntities :: EntityMap,
        epFrameworkEntities :: EntityMap,
        epNextID :: Int
    }
    deriving ( Read, Show, Typeable, Data )

emptyEntityPile :: EntityPile
emptyEntityPile = EntityPile Map.empty Map.empty 1

addImportedEntities :: ModuleName -> EntityMap
                    -> EntityPile -> EntityPile
addImportedEntities _mod entities pile
    = pile { epFrameworkEntities = entities `Map.union` epFrameworkEntities pile }

newEntity :: MonadState EntityPile m => Entity -> m EntityID
newEntity e
    = do
        EntityPile entities fws newID <- get
        let theID = LocalEntity newID
        put (EntityPile (Map.insert theID e entities) fws (newID + 1))
        return theID
        
lookupEntity :: String -> EntityID -> EntityPile -> Entity
lookupEntity loc theID entityPile
    = case Map.lookup theID eMap of
        Just e -> e
        Nothing -> error $ "lookupEntity " ++ loc ++ " " ++ show theID
    where
        eMap = case theID of
            LocalEntity _ -> epEntities entityPile
            FrameworkEntity _ _ -> epFrameworkEntities entityPile

hasEntity :: EntityID -> EntityPile -> Bool
hasEntity theID entityPile
    = Map.member theID eMap
    where
        eMap = case theID of
            LocalEntity _ -> epEntities entityPile
            FrameworkEntity _ _ -> epFrameworkEntities entityPile

makeEntityPileIndex :: Ord a => (Entity -> a) -> EntityPile -> Map.Map a EntityID
makeEntityPileIndex f entityPile
    = Map.fromList [ (f entity, id)
                   | (id, entity) <- entityPileToList entityPile ]

makeEntityPileMultiIndex :: Ord a => (Entity -> a) -> EntityPile -> Map.Map a (Set.Set EntityID)
makeEntityPileMultiIndex f entityPile
    = Map.fromListWith Set.union
        [ (f entity, Set.singleton id)
        | (id, entity) <- entityPileToList entityPile ]

makeEntityPileLocalMultiIndex :: Ord a => (Entity -> a) -> EntityPile -> Map.Map a (Set.Set EntityID)
makeEntityPileLocalMultiIndex f entityPile
    = Map.fromListWith Set.union
        [ (f entity, Set.singleton id)
        | (id, entity) <- Map.toList $ epEntities entityPile ]

entityPileToList :: EntityPile -> [(EntityID, Entity)]
entityPileToList pile
    = Map.toList $ epEntities pile `Map.union` epFrameworkEntities pile

transformLocalEntities :: (EntityMap -> EntityMap)
                       -> EntityPile -> EntityPile
transformLocalEntities f pile
    = pile { epEntities = f (epEntities pile) }

localEntities, frameworkEntities :: EntityPile -> EntityMap
localEntities = epEntities
frameworkEntities = epFrameworkEntities

replaceLocalEntities :: EntityMap -> EntityPile -> EntityPile
replaceLocalEntities locals = transformLocalEntities (const locals)

reportProgressForPile :: ProgressReporter -> EntityPile -> EntityPile
reportProgressForPile pr = transformLocalEntities (reportProgressForMap pr)
