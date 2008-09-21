module BinaryInstances where

import Entities
import SyntaxTree
import CTypeToHaskell
import Data.Binary


instance Binary SyntaxTree.EnumValue where
  put NextValue = putWord8 0
  put (GivenValue a) = putWord8 1 >> put a
  put (TooComplicatedValue a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return NextValue
      1 -> get >>= \a -> return (GivenValue a)
      2 -> get >>= \a -> return (TooComplicatedValue a)
      _ -> fail "no parse"

instance Binary SyntaxTree.CType where
  put (CTIDType a) = putWord8 0 >> put a
  put (CTSimple a) = putWord8 1 >> put a
  put (CTPointer a) = putWord8 2 >> put a
  put CTUnknown = putWord8 3
  put (CTEnum a b) = putWord8 4 >> put a >> put b
  put (CTStruct a b) = putWord8 5 >> put a >> put b
  put (CTUnion a b) = putWord8 6 >> put a >> put b
  put (CTBuiltin a b c) = putWord8 7 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CTIDType a)
      1 -> get >>= \a -> return (CTSimple a)
      2 -> get >>= \a -> return (CTPointer a)
      3 -> return CTUnknown
      4 -> get >>= \a -> get >>= \b -> return (CTEnum a b)
      5 -> get >>= \a -> get >>= \b -> return (CTStruct a b)
      6 -> get >>= \a -> get >>= \b -> return (CTUnion a b)
      7 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CTBuiltin a b c)
      _ -> fail "no parse"

instance Binary SyntaxTree.Length where
  put LongLong = putWord8 0
  put Long = putWord8 1
  put Short = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return LongLong
      1 -> return Long
      2 -> return Short
      _ -> fail "no parse"

instance Binary SyntaxTree.Selector where
  put (Selector a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Selector a b c d)

instance Binary CTypeToHaskell.HSelectorType where
  put (HSelectorType a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (HSelectorType a b c d)

instance Binary CTypeToHaskell.HType where
  put (HType a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (HType a b c)

instance Binary CTypeToHaskell.HTypeTerm where
  put (Con a) = putWord8 0 >> put a
  put (a :$ b) = putWord8 1 >> put a >> put b
  put (Var a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Con a)
      1 -> get >>= \a -> get >>= \b -> return (a :$ b)
      2 -> get >>= \a -> return (Var a)
      _ -> fail "no parse"

instance Binary Entities.EntityID where
  put (LocalEntity a) = putWord8 0 >> put a
  put (FrameworkEntity a b) = putWord8 1 >> put a >> put b
  put (DelayedClassLookup a) = putWord8 2 >> put a
  put (DelayedProtocolLookup a) = putWord8 3 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (LocalEntity a)
      1 -> get >>= \a -> get >>= \b -> return (FrameworkEntity a b)
      2 -> get >>= \a -> return (DelayedClassLookup a)
      3 -> get >>= \a -> return (DelayedProtocolLookup a)
      _ -> fail "no parse"

instance Binary Entities.Module where
  put (LocalModule a) = putWord8 0 >> put a
  put (FrameworkModule a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (LocalModule a)
      1 -> get >>= \a -> get >>= \b -> return (FrameworkModule a b)
      _ -> fail "no parse"

instance (Binary a, Binary b) => Binary (Entities.HaskellType a b) where
  put (ConvertedType a b) = putWord8 0 >> put a >> put b
  put (UnconvertedType a) = putWord8 1 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (ConvertedType a b)
      1 -> get >>= \a -> return (UnconvertedType a)
      _ -> fail "no parse"

instance Binary Entities.EntityInfo where
  put (ClassEntity a) = putWord8 0 >> put a
  put (TypeSynonymEntity a) = putWord8 1 >> put a
  put (EnumEntity a b) = putWord8 2 >> put a >> put b
  put AdditionalTypeEntity = putWord8 3
  put (SelectorEntity a) = putWord8 4 >> put a
  put (ProtocolEntity a b) = putWord8 5 >> put a >> put b
  put MethodEntity = putWord8 6
  put ProtocolAdoptionEntity = putWord8 7
  put (ExternVarEntity a) = putWord8 8 >> put a
  put (ExternFunEntity a) = putWord8 9 >> put a
  put (ReexportEntity a) = putWord8 10 >> put a
  put (AdditionalCodeEntity a b c d) = putWord8 11 >> put a >> put b >> put c >> put d
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ClassEntity a)
      1 -> get >>= \a -> return (TypeSynonymEntity a)
      2 -> get >>= \a -> get >>= \b -> return (EnumEntity a b)
      3 -> return AdditionalTypeEntity
      4 -> get >>= \a -> return (SelectorEntity a)
      5 -> get >>= \a -> get >>= \b -> return (ProtocolEntity a b)
      6 -> return MethodEntity
      7 -> return ProtocolAdoptionEntity
      8 -> get >>= \a -> return (ExternVarEntity a)
      9 -> get >>= \a -> return (ExternFunEntity a)
      10 -> get >>= \a -> return (ReexportEntity a)
      11 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (AdditionalCodeEntity a b c d)
      _ -> fail "no parse"

instance Binary Entities.Name where
  put (CName a) = putWord8 0 >> put a
  put (ProtocolName a) = putWord8 1 >> put a
  put (SelectorName a) = putWord8 2 >> put a
  put (ProtocolAdoptionName a b) = putWord8 3 >> put a >> put b
  put (SelectorInstanceName a b c) = putWord8 4 >> put a >> put b >> put c
  put Anonymous = putWord8 5
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (CName a)
      1 -> get >>= \a -> return (ProtocolName a)
      2 -> get >>= \a -> return (SelectorName a)
      3 -> get >>= \a -> get >>= \b -> return (ProtocolAdoptionName a b)
      4 -> get >>= \a -> get >>= \b -> get >>= \c -> return (SelectorInstanceName a b c)
      5 -> return Anonymous
      _ -> fail "no parse"

instance Binary Entities.Entity where
  put (Entity a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Entity a b c d e)

instance Binary Entities.EntityPile where
  put (EntityPile a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (EntityPile a b c)

instance Binary CTypeToHaskell.SelectorKind where
  put PlainSelector = putWord8 0
  put CovariantSelector = putWord8 1
  put CovariantInstanceSelector = putWord8 2
  put AllocSelector = putWord8 3
  put InitSelector = putWord8 4
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return PlainSelector
      1 -> return CovariantSelector
      2 -> return CovariantInstanceSelector
      3 -> return AllocSelector
      4 -> return InitSelector
      _ -> fail "no parse"
