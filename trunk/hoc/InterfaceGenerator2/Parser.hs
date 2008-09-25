module Parser where

import Data.Maybe(catMaybes, isJust)
import Data.Char(ord, isUpper, isDigit)
import Data.Bits(shiftL)
import Control.Monad(guard)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Text.ParserCombinators.Parsec.Expr

import SyntaxTree

import qualified Data.Map as Map

objcDef = emptyDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = False
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> char '_'
    , reservedNames  = ["@class","@protocol","@interface","@implementation","@end","@property",
                        "const", "volatile", "struct", "union", "enum",
                        "@required", "@optional"]
    , caseSensitive  = True
    }

objc :: TokenParser ()
objc = makeTokenParser objcDef

    
header = try (whiteSpace objc >> eof >> return []) <|> (
                fmap catMaybes $ many (whiteSpace objc >>
                                    (try interestingThing
                                    <|> uninterestingThing))
            )

uninterestingThing = skipMany1 (satisfy (\x -> x /= '@' && x /= ';')) >> return Nothing

interestingThing =
        class_decl
    <|> (try protocol_decl)
    <|> interface_decl
    <|> empty_decl
    <|> (fmap Just type_declaration)
    <|> (fmap Just extern_decl)

empty_decl = semi objc >> return Nothing

class_decl = do
    reserved objc "@class"
    classes <- commaSep1 objc (identifier objc)
    semi objc
    return $ Just $ ForwardClass classes    

protocol_decl = do
    reserved objc "@protocol"
    protos <- commaSep1 objc (identifier objc)
    semi objc
    return $ Just $ ForwardProtocol protos

interface_decl = do
    proto <- (reserved objc "@interface" >> return False)
            <|> (reserved objc "@protocol" >> return True)
    class_name <- identifier objc
    what <- if proto
        then do
            protos <- protocol_spec
            return $ Protocol class_name protos 
        else (do
            cat_name <- category_spec
            protos <- protocol_spec
            return $ Category class_name cat_name protos
        ) <|> (do
            super <- superclass_spec
            protos <- protocol_spec
            return $ Interface class_name super protos
        )
    instance_variables
    selectors <- many selectorListItem
    reserved objc "@end"
    return $ Just $ SelectorList what selectors
    
category_spec = parens objc (identifier objc)
    
superclass_spec = (do
        colon objc
        superclass <- identifier objc
        return $ Just superclass
    ) <|> return Nothing
    
protocol_spec = 
    angles objc (commaSep1 objc (identifier objc))
    <|> return []
    
instance_variables = skipBlock <|> return ()

selectorListItem
    =   selector 
    <|> (fmap LocalDecl type_declaration)
    <|> fmap LocalDecl extern_decl
    <|> property
    <|> requiredOrOptional

requiredOrOptional
    =   (reserved objc "@required" >> return (Required True))
    <|> (reserved objc "@optional" >> return (Required False))

selector = do
    classOrInstanceMethod <-
            (symbol objc "-" >> return InstanceMethod)
        <|> (symbol objc "+" >> return ClassMethod)
    -- str <- many (satisfy (\c -> c /= ';' && c /= '@'))
    rettype <- type_spec
    (name,types,vararg) <- (
            do
                manythings <- many1 (try $ do
                        namePart <- identifier objc <|> return ""
                        colon objc
                        argType <- type_spec
                        argName <- identifier objc
                        return (namePart, argType)
                    )
                vararg <- (symbol objc "," >> symbol objc "..." >> return True) <|> return False
                let (nameParts,types) = unzip manythings
                return (concat $ map (++":") nameParts , types, vararg)
        ) <|> (
            do
                name <- identifier objc
                return (name,[],False)
        )
    availability
    semi objc
    return (classOrInstanceMethod $ Selector name rettype types vararg)

property
    = do
        reserved objc "@property"
        optional (parens objc (identifier objc))
        basetype <- type_no_pointers
        args <- commaSep objc varname_with_stars
        semi objc
        return PropertyDecl

-- type_spec = parens objc ctype <|> return CTNoType
    -- where
    --  ctype = fmap (CType . unwords) $ many (identifier objc <|> symbol objc "*")

type_spec = try (parens objc ctype) <|> (skipParens >> return CTUnknown) <|> return (CTIDType [])

type_no_pointers = do  -- "const char" in "const char *foo[32]"
    many ignored_type_qualifier   -- ignore
    simple_type

varname_with_stars = do
    pointers_and_such <- many type_operator
    name <- identifier objc
    arrays <- many (symbol objc "[" >> symbol objc "]" >> return CTPointer)
    return (name, \t -> foldl (flip ($)) t (pointers_and_such ++ arrays))

ctype = do
    simple <- type_no_pointers
    pointers_and_such <- many type_operator
    return $ foldl (flip ($)) simple pointers_and_such
    
simple_type = id_type <|> enum_type <|> struct_type <|> try builtin_type <|> fmap CTSimple (identifier objc)

builtin_type = do
    signedness <- (reserved objc "signed" >> return (Just True))
                <|> (reserved objc "unsigned" >> return (Just False))
                <|> return Nothing
    length <- (try (reserved objc "long" >> reserved objc "long") >> return (Just LongLong))
                <|> (reserved objc "long" >> return (Just Long))
                <|> (reserved objc "short" >> return (Just Short))
                <|> return Nothing
    key <- if isJust signedness || isJust length
        then option "int" (try simple_builtin)
        else simple_builtin
    return $ CTBuiltin signedness length key
    
simple_builtin = do
    typ <- identifier objc
    if typ `elem` ["char","short","int","float","double"]
        then return typ
        else fail "not a built-in type"
        
id_type = do
    reserved objc "id"
    protos <- protocol_spec
    return $ CTIDType protos
        
multiCharConstant =
        lexeme objc (between (char '\'') (char '\'') multiChars)
    where
        multiChars = do
            chars <- many1 (satisfy (/= '\''))
            return $ sum $ zipWith (*)  
                (map (fromIntegral.ord) $ reverse chars)
                (iterate (*256) 1)


const_int_expr env = buildExpressionParser optable basic
    where
        basic = (integer objc) <|> multiCharConstant
            <|> (do name <- identifier objc
                    Map.lookup name env)
        optable = [ [Infix shiftLeft AssocLeft] ]
        
        shiftLeft = op "<<" (flip $ flip shiftL . fromIntegral)
        
        op str f = reservedOp objc str >> return f
        
enum_type =
    do
        key <- reserved objc "enum"
        id <- identifier objc <|> return ""
        body <- braces objc (enum_body Map.empty (-1)) <|> return []
        return $ CTEnum id body
    where
        enum_body env lastVal = do
            id <- identifier objc
            val <- (do
                    symbol objc "="
                    const_int_expr env
                ) <|> return (lastVal + 1)
            
            let env' = Map.insert id val env
            xs <- option [] $ comma objc >> option [] (enum_body env' val)
            return $ (id, GivenValue val) : xs
    
struct_type =
    do
        key <- (reserved objc "struct" >> return CTStruct)
            <|> (reserved objc "union" >> return CTUnion)
        id <- identifier objc <|> return ""
        body <- braces objc struct_union_body <|> return []
        return $ key id body
    where
        struct_union_body = many member
        member = do 
            typ <- ctype
            name <- identifier objc
            semi objc
            return (typ, name)
        
type_operator = 
    (symbol objc "*" >> return CTPointer)
    <|> (ignored_type_qualifier >> return id)
            
ignored_type_qualifier = 
        reserved objc "const"
    <|> reserved objc "volatile"
    <|> reserved objc "in"
    <|> reserved objc "out"
    <|> reserved objc "inout"
    <|> reserved objc "bycopy"
    <|> reserved objc "byref"
    <|> reserved objc "oneway"
            
typedef = do
    reserved objc "typedef"
    oldType <- ctype
    newType <- identifier objc
    semi objc
    return $ Typedef oldType newType
    
ctypeDecl = do
    typ <- enum_type <|> struct_type
    semi objc
    return $ CTypeDecl typ

type_declaration = typedef <|> ctypeDecl

extern_decl =
    extern_keyword >> ctype >>= \t -> identifier objc >>= \n ->
        do 
            args <- parens objc (commaSep objc argument)
            availability
            semi objc
            return $ ExternFun (Selector n t args False)
    <|> do
            availability
            semi objc
            return $ ExternVar t n
    where
        argument = do t <- ctype
                      optional (identifier objc)
                      arrays <- many (symbol objc "[" >> symbol objc "]" >> return CTPointer)
                      return $ foldl (flip ($)) t arrays
                      
availability = optional $
    do x <- identifier objc
       guard $ all (\c -> isUpper c || isDigit c || c == '_') x
       -- guard (any (`isPrefixOf` x) ["AVAILABLE_MAC_", "DEPRECATED_IN_"])
        
extern_keyword =
        reserved objc "extern"
    <|> reserved objc "FOUNDATION_EXPORT" -- N.B. "Export" vs. "Extern".
    <|> reserved objc "APPKIT_EXTERN"
    <|> reserved objc "GS_EXPORT"

skipParens = parens objc (skipMany (
    (satisfy (\x -> x /= '(' && x /= ')') >> return ())
    <|> skipParens
    ))

skipBlock = braces objc (skipMany (
    (satisfy (\x -> x /= '{' && x /= '}') >> return ())
    <|> skipBlock
    ))

