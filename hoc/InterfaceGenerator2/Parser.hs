{-# LANGUAGE TypeSynonymInstances #-}
module Parser( Parser, header, selector ) where

import Data.Maybe(catMaybes, isJust, fromJust)
import Data.Char(ord, isUpper, isDigit)
import Data.Bits(shiftL, (.|.))
import Control.Monad(guard)

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Expr

import SyntaxTree

import qualified Data.Map as Map

import Control.Monad.Trans( lift )
import Messages

import qualified Text.PrettyPrint.HughesPJ as PP

type Parser a = ParsecT String () Messages a

-- type Parser a = forall b. ParsecT String b Messages a


objcDef = LanguageDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = False
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> char '_'
    , opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames  = ["@class","@protocol","@interface","@implementation","@end","@property",
                        "const", "volatile", "struct", "union", "enum", "typedef",
                        "__attribute__", "__strong",
                        "@required", "@optional", "@private", "@public" ]
    , caseSensitive  = True
    }

objc :: GenTokenParser String () Messages
objc = makeTokenParser objcDef

singleton x = [x]

parseWarning :: String -> Parser ()
parseWarning msg
    = do
        pos <- getPosition
        lift (message $ PP.text (show pos ++ ": " ++ msg))

header :: Parser [Declaration]
    
header = do
    optional (whiteSpace objc)
    things <- fmap concat $ many $ do
        -- thing <- try interestingThing <|> uninterestingThing -- lenient parsing
        thing <- interestingThing   -- strict parsing
        optional (whiteSpace objc)
        return thing
    eof
    return things

uninterestingThing :: Parser (Maybe Declaration)
uninterestingThing = skipMany1 (satisfy (\x -> x /= '@' && x /= ';')) >> return Nothing

interestingThing =
        ignoredToplevelThing
    <|> class_decl
    <|> (try protocol_decl)
    <|> interface_decl
    <|> empty_decl
    <|> type_declaration
    <|> inline_function
    <|> extern_decl
    <|> (semi objc >> return [])

ignoredToplevelThing
    = (foldl1 (<|>) . map (reserved objc))
      ["CA_EXTERN_C_BEGIN", "CA_EXTERN_C_END",
       "CF_EXTERN_C_BEGIN", "CF_EXTERN_C_END"]
      >> return []
--

skipParens = parens objc (skipMany (
    (satisfy (\x -> x /= '(' && x /= ')') >> return ())
    <|> skipParens
    ))

skipBlockContents = (skipMany (
    (satisfy (\x -> x /= '{' && x /= '}') >> return ())
    <|> skipBlock
    ))
skipBlock = braces objc skipBlockContents

skipEnumValue = skipMany1 (satisfy (\x -> x /= '}' && x /= ','))

-- Plain C

empty_decl :: Parser [a]
empty_decl = semi objc >> return []


const_int_expr :: Map.Map String Integer -> Parser Integer
const_int_expr env = expr
    where
        expr = buildExpressionParser optable basic

        basic = suffixedInteger 
            <|> multiCharConstant
            <|> definedConstant
            <|> parens objc expr

        optable = [ [Infix (op "<<" (cast2nd shiftL))   AssocLeft],
                    [Infix (op "|" (.|.))               AssocLeft] ]
            where        
                op str f = reservedOp objc str >> return f
                cast2nd f x y = f x (fromIntegral y)
                
        suffixedInteger =
            do
                val <- integer objc
                optional (reserved objc "U" <|> reserved objc "L"
                            <|> reserved objc "UL") -- ### TODO: no space allowed before 'U'
                return val

        multiCharConstant =
                lexeme objc (between (char '\'') (char '\'') multiChars)
            where
                multiChars = do
                    chars <- many1 (satisfy (/= '\''))
                    return $ sum $ zipWith (*)  
                        (map (fromIntegral.ord) $ reverse chars)
                        (iterate (*256) 1)
                
        definedConstant =
            do
                name <- identifier objc
                Map.lookup name env <|> (parseWarning (name ++ " undefined") >> fail "")

-- A ctype is a complete C type, as you'd write it in a cast expression.
-- Examples include "int", "const char*", and "void (*)(int, float[3])"
ctype = do
    simple <- simple_type
    (_, f) <- declarator True (return ())
    return (f simple)

-- A simple_type is a C type without any pointers, arrays or functions.
-- (but including struct, enum and union declarations).
-- If you declare multiple variables in one C declaration, the simple_type
-- is what they have in common.
-- Examples include "int", "const char", "char const", "struct {int *x;}",
-- but NOT "const char*".

simple_type = do  -- "const char" in "const char *foo[32]"
    many ignored_type_qualifier   -- ignore
    t <-    id_type
        <|> enum_type
        <|> struct_type
        <|> try builtin_type
        <|> do n <- identifier objc
               protos <- protocol_spec     -- TOOD: use these protocols
               return $ CTSimple n
    many ignored_type_qualifier
    return t

ignored_type_qualifier = 
        reserved objc "const"
    <|> reserved objc "volatile"
    <|> reserved objc "in"
    <|> reserved objc "out"
    <|> reserved objc "inout"
    <|> reserved objc "bycopy"
    <|> reserved objc "byref"
    <|> reserved objc "oneway"
    <|> reserved objc "__strong"

-- An id_declarator is an identifier surrounded by things like "*", "[]" and
-- function arguments.
-- In a declaration of a single C variable or function, everything except the
-- simple_type is part of the id_declarator.
-- In the declaration "const char *x[32]", "*x[32]" is the id_declarator.
-- The parser returns the identifier and a function that transforms the
-- simple_type ("const char" in the example) to the type of the identifier
-- ("const char * [32]" in the example).

id_declarator :: Parser (String, CType -> CType)
id_declarator = declarator False (identifier objc)

declarator :: Bool -> Parser a -> Parser (a, CType -> CType)
declarator emptyDeclaratorPossible thing = do
        prefixes <- many prefix_operator
        (name, typeFun) <- terminal
        postfixes <- many postfix_operator
        return (name, foldl (.) typeFun (postfixes ++ prefixes))
    where
        mbTry | emptyDeclaratorPossible = try
              | otherwise = id
        terminal = 
               mbTry (parens objc (declarator emptyDeclaratorPossible thing))
               <|> (thing >>= \name -> return (name, id))
        prefix_operator =
            do
                symbol objc "*"
                many ignored_type_qualifier
                return CTPointer

        postfix_operator =
            brackets objc (optional (integer objc) >> return CTPointer)
            <|> do
                (args, vararg) <- parens objc arguments
                return (\retval -> CTFunction retval args vararg)
       
        arguments =
            do
                args <- commaSep objc argument
                case reverse args of
                    (Nothing : moreArgs)
                        | all isJust moreArgs ->
                            return (map fromJust $ reverse moreArgs, True)
                    _   | all isJust args -> return (map fromJust args, False)
                        | otherwise -> fail "'...' in the middle of argument list"
            where
                argument = 
                    (symbol objc "..." >> return Nothing)
                    <|> do
                        t <- simple_type
                        (_, tf) <- declarator True (optional $ identifier objc)
                        return $ Just $ tf t
    


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
        

        
enum_type =
    do
        key <- reserved objc "enum"
        id <- identifier objc <|> return ""
        body <- braces objc (enum_body Map.empty (Just (-1))) <|> return []
        return $ CTEnum id body
    where
        enum_body env lastVal = do
            id <- identifier objc
            mbVal <- (do
                    symbol objc "="
                    try (fmap Just $ const_int_expr env)
                        <|> (skipEnumValue >> return Nothing)
                ) <|> return (lastVal >>= Just . (+1))
            
            case mbVal of
                Just val -> do 
                    let env' = Map.insert id val env
                    xs <- option [] $ comma objc
                                    >> option [] (enum_body env' (Just val))
                    return $ (id, GivenValue val) : xs
                Nothing -> do 
                    parseWarning $ "Couldn't handle enum value for " ++ id
                    xs <- option [] $ comma objc
                                    >> option [] (enum_body env Nothing)
                    return $ (id, TooComplicatedValue "") : xs
    
struct_type =
    do
        key <- (reserved objc "struct" >> return CTStruct)
            <|> (reserved objc "union" >> return CTUnion)
        id <- identifier objc <|> return ""
        body <- fmap concat $ braces objc struct_union_body <|> return []
        return $ key id body
    where
        struct_union_body = try (many member)
            <|> (skipBlockContents >> parseWarning "problem parsing struct" >> return [])
        member = do 
            typ <- simple_type
            things <- commaSep objc $ do
                (name, typeModifiers) <- id_declarator
                bitfield <- option Nothing
                    (symbol objc ":" >> integer objc >>= return . Just)
                return (name, typeModifiers)
            availability
            semi objc
            return [ (modifier typ, name) | (name, modifier) <- things ]
            
            
typedef = do
    reserved objc "typedef"
    baseType <- simple_type
    
    newTypes <- commaSep objc id_declarator
    availability
    semi objc
    return $ [Typedef (typeFun baseType) name
             | (name, typeFun) <- newTypes ]

ctypeDecl = do
    typ <- enum_type <|> struct_type
    availability
    semi objc
    return [CTypeDecl typ]

type_declaration = typedef <|> ctypeDecl

extern_decl =
    do
        optional extern_keyword
        t <- simple_type
        vars <- commaSep objc (one_var t)
        availability
        semi objc
        return vars
    where
        one_var t = do
            (n, typeOperators) <- id_declarator
            return $ case typeOperators t of
                CTFunction retval args varargs
                    -> ExternFun (Selector n retval args varargs)
                otherType
                    -> ExternVar otherType n
           
extern_keyword =
        reserved objc "extern"
    <|> reserved objc "FOUNDATION_EXPORT" -- N.B. "Export" vs. "Extern".
    <|> reserved objc "APPKIT_EXTERN"
    <|> reserved objc "GS_EXPORT"
    <|> reserved objc "CA_EXTERN"
    <|> reserved objc "CF_EXPORT"
    <|> reserved objc "COREDATA_EXTERN"
    
inline_function =
    do
        reserved objc "inline" <|> reserved objc "NS_INLINE" 
                               <|> reserved objc "CF_INLINE"
        t <- simple_type
        (n, tf) <- id_declarator
        skipBlock
        return []


-- Ignore __attribute__((...)) and Apple's countless different availability macros,
-- which all expand to some __attribute__. My favourite example is
-- "AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3"

availability :: Parser ()
availability = fmap (const ()) $ many $
    do reserved objc "__attribute__"
       parens objc (skipParens)
       return ()
    <|>
    do x <- identifier objc
       guard $ all (\c -> isUpper c || isDigit c || c == '_') x
       -- guard (any (`isPrefixOf` x) ["AVAILABLE_MAC_", "DEPRECATED_IN_"])


-- Objective C

class_decl = do
    reserved objc "@class"
    classes <- commaSep1 objc (identifier objc)
    semi objc
    return [ForwardClass classes]

protocol_decl = do
    reserved objc "@protocol"
    protos <- commaSep1 objc (identifier objc)
    semi objc
    return [ForwardProtocol protos]

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
        selectors <- fmap concat $ many selectorListItem
        reserved objc "@end"
        return [SelectorList what selectors]
    where    
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
    =   fmap singleton selector 
    <|> fmap (map LocalDecl) type_declaration
    <|> fmap (map LocalDecl) extern_decl
    <|> property_declaration
    <|> fmap singleton requiredOrOptional
    <|> (semi objc >> return [])

requiredOrOptional
    =   (reserved objc "@required" >> return (Required True))
    <|> (reserved objc "@optional" >> return (Required False))

selector = do
    classOrInstanceMethod <-
            (symbol objc "-" >> return InstanceMethod)
        <|> (symbol objc "+" >> return ClassMethod)
    -- str <- many (satisfy (\c -> c /= ';' && c /= '@'))
    rettype <- option (CTIDType []) (parens objc ctype)
    (name,types,vararg) <- (
            do
                manythings <- many1 (try $ do
                        namePart <- identifier objc <|> return ""
                        colon objc
                        argType <- option (CTIDType []) (parens objc ctype)
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

property_declaration
    = do
        reserved objc "@property"
        properties <- option [] (parens objc (commaSep objc $ property_attribute))
        basetype <- simple_type
        things <- commaSep objc id_declarator
        availability
        semi objc
        return $ map (\ (name, typeModifiers) -> PropertyDecl $
                        Property (typeModifiers $ basetype)
                                 name properties ) things
    where
        property_attribute = 
                (do reserved objc "getter"
                    symbol objc "="
                    name <- identifier objc
                    return $ Getter name)
            <|> (do reserved objc "setter"
                    symbol objc "="
                    name <- identifier objc
                    symbol objc ":"
                    return $ Setter (name ++ ":"))
            <|> (reserved objc "readonly" >> return ReadOnly)
            <|> (reserved objc "readwrite" >> return ReadWrite)
            <|> (reserved objc "assign" >> return Assign)
            <|> (reserved objc "retain" >> return Retain)
            <|> (reserved objc "copy" >> return Copy)

