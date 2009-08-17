{-# LANGUAGE CPP, TemplateHaskell, RelaxedPolyRec #-}
module Traversals where

#define TEMPLATES 1

import Entities
import Control.Exception

#if TEMPLATES
import THTraversal
#else
import Data.Generics
#endif

mapEntityIDs :: (EntityID -> EntityID) -> Entity -> Entity
#if TEMPLATES
mapEntityIDs f =
    $(makeEverywhere [t| Entity |]
                [ ( [t| EntityID |], [| f |] ) ])
#else
mapEntityIDs f = everywhere (mkT f)
#endif

mapEntityIDsAndModules :: (EntityID -> EntityID)
                       -> (Module -> Module)
                       -> Entity -> Entity

#if TEMPLATES
mapEntityIDsAndModules f g
    = $(makeEverywhere [t| Entity |]
                [ ( [t| EntityID |], [| f |] ),
                  ( [t| Module |],   [| g |] ) ])
#else
mapEntityIDsAndModules f g = everywhere (mkT f `extT` g)
#endif


mapEntityIDsInEntity :: (EntityID -> EntityID) -> Entity -> Entity
#if TEMPLATES
mapEntityIDsInEntity f
    = $(makeEverywhere [t| Entity |]
                [ ( [t| EntityID |], [| f |] ) ])
#else
mapEntityIDsInEntity f = everywhere (mkT f)
#endif


mapTypes :: (HaskellValueType -> HaskellValueType)
         -> (HaskellSelectorType -> HaskellSelectorType)
         -> Entity -> Entity

#if TEMPLATES
mapTypes val sel
    = $(makeEverywhere [t| Entity |]
                [ ( [t| HaskellValueType |], [| val |] ),
                  ( [t| HaskellSelectorType |], [| sel |] ) ])
#else
mapTypes val sel = everywhere (mkT val `extT` sel)
#endif


foreachTypeInEntity :: Monad m
                    => (HaskellValueType -> m ())
                    -> (HaskellSelectorType -> m ())
                    -> Entity
                    -> m ()

#if TEMPLATES
foreachTypeInEntity val sel
    = $(makeForeachM [t| Entity |]
        [ ([t| HaskellValueType |], [| val |]),
          ([t| HaskellSelectorType |], [| sel |]) ])

#else
foreachTypeInEntity val sel e
    = do
        everywhereM (mkM (\x -> sel x >> return x)
                    `extM` (\x -> val x >> return x))
                    e
        return ()

#endif

foreachIDInEntity :: Monad m
                  => (EntityID -> m ())
              -> Entity
              -> m ()
#if TEMPLATES
foreachIDInEntity f
    = $(makeForeachM [t| Entity |]
        [ ([t| EntityID |], [| f |]) ])

#else
foreachIDInEntity f e
    = do
        everywhereM (mkM (\x -> f x >> return x))
                    e
        return ()

#endif

mentionedEntityIDs :: Entity -> [EntityID]

#if TEMPLATES
mentionedEntityIDs
    = $(makeTraverser [t| Entity |]
                      [| const [] |]
                      [| (++) |]
                      [| const [] |]
                      [ ( [t| EntityID |], [| \x -> [x] |] ) ]
        )
#else
mentionedEntityIDs = listify (const True)
#endif

deepEvaluateEntity :: Entity -> IO ()

#if TEMPLATES
deepEvaluateEntity
    = $(makeTraverser [t| Entity |]
                      [| const (return ()) |]
                      [| flip (>>) |]
                      [| \x -> evaluate x >> return () |]
                      [])
#else
deepEvaluateEntity x
    = everywhereM evaluate x >> return ()
#endif

