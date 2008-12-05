{-# LANGUAGE DeriveDataTypeable #-}
module SyntaxTree where

import Data.Generics
import SrcPos

type ParsedHeader = [ DeclarationAndPos ]

data Declaration =
        ForwardClass [String]
    |   ForwardProtocol [String]
    |   SelectorList SelectorListHeader [(SrcPos, SelectorListItem)]
    |   Typedef CType String
    |   CTypeDecl CType
    |   ExternVar CType String
    |   ExternFun Selector
    deriving (Show,Eq,Ord)

type DeclarationAndPos = (SrcPos, Declaration)

data SelectorListHeader = 
        Interface String (Maybe String) [String]
    |   Protocol String [String]
    |   Category String String [String]
    deriving (Show,Eq,Ord)

data SelectorListItem =
        InstanceMethod Selector
    |   ClassMethod Selector
    |   LocalDecl Declaration
    |   PropertyDecl CType String [PropertyAttribute]
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
    
data PropertyAttribute =
        Getter String
    |   Setter String
    |   ReadOnly
    |   ReadWrite
    |   Assign
    |   Retain
    |   Copy
    deriving (Show, Eq, Ord)
                       
    
data EnumValue = NextValue | GivenValue Integer | TooComplicatedValue String
    deriving (Read, Show, Eq, Ord,Typeable,Data)
    
data CType = CTIDType [String {- protocols -}]
           | CTSimple String
           | CTPointer CType
           | CTFunction CType [CType] Bool
           | CTUnknown
           | CTEnum String [(String, EnumValue)]
           | CTStruct String [(CType, String)]
           | CTUnion String [(CType, String)]
           | CTBuiltin (Maybe Bool {- signed/unsigned? -}) (Maybe Length) String
    deriving (Read,Show,Eq,Ord,Typeable,Data)
    
data Length = LongLong | Long | Short
    deriving (Read,Show,Eq,Ord,Typeable,Data)

cTypeInt = CTBuiltin Nothing Nothing "int"