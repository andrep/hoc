{-# OPTIONS -fglasgow-exts #-}
module SyntaxTree where

import Data.Generics

data Declaration =
        ForwardClass [String]
    |   ForwardProtocol [String]
    |   SelectorList SelectorListHeader [SelectorListItem]
    |   Typedef CType String
    |   CTypeDecl CType
    |   ExternVar CType String
    |   ExternFun Selector
    deriving (Show,Eq,Ord)

data SelectorListHeader = 
        Interface String (Maybe String) [String]
    |   Protocol String [String]
    |   Category String String [String]
    deriving (Show,Eq,Ord)

data SelectorListItem =
        InstanceMethod Selector
    |   ClassMethod Selector
    |   LocalDecl Declaration
    |   PropertyDecl
    |   Required Bool
    deriving (Show,Eq,Ord)

data Selector =
        Selector {
            selName :: String,
            selRetType :: CType,
            selArgTypes :: [CType],
            selVarArg :: Bool
        }
    deriving (Read,Show,Eq,Ord,Typeable,Data)
    
data EnumValue = NextValue | GivenValue Integer | TooComplicatedValue String
    deriving (Read, Show, Eq, Ord,Typeable,Data)
    
data CType = CTIDType [String {- protocols -}]
           | CTSimple String
           | CTPointer CType
           | CTUnknown
           | CTEnum String [(String, EnumValue)]
           | CTStruct String [(CType, String)]
           | CTUnion String [(CType, String)]
           | CTBuiltin (Maybe Bool {- signed/unsigned? -}) (Maybe Length) String
    deriving (Read,Show,Eq,Ord,Typeable,Data)
    
data Length = LongLong | Long | Short
    deriving (Read,Show,Eq,Ord,Typeable,Data)

cTypeInt = CTBuiltin Nothing Nothing "int"