module Parser where

import Data.Maybe(catMaybes, isJust)
import Data.Char(ord)
import Data.Bits(shiftL)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Text.ParserCombinators.Parsec.Expr

import SyntaxTree

objcDef = emptyDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = False
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> char '_'
    , reservedNames  = ["@class","@protocol","@interface","@implementation","@end",
                        "const", "volatile", "struct", "union", "enum"]
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
    <|> extern_decl

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

selectorListItem = selector <|> (fmap LocalDecl type_declaration)

selector = do
    classOrInstanceMethod <-
            (symbol objc "-" >> return InstanceMethod)
        <|> (symbol objc "+" >> return ClassMethod)
    -- str <- many (satisfy (\c -> c /= ';' && c /= '@'))
    rettype <- type_spec
    (name,types,vararg) <- try (
            do
                manythings <- many (do
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
    semi objc
    return (classOrInstanceMethod $ Selector name rettype types vararg)

-- type_spec = parens objc ctype <|> return CTNoType
    -- where
    --  ctype = fmap (CType . unwords) $ many (identifier objc <|> symbol objc "*")

type_spec = try (parens objc ctype) <|> (skipParens >> return CTUnknown) <|> return (CTIDType [])



ctype = do
    many ignored_type_qualifier   -- ignore
    simple <- simple_type
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
        
const_int_expr = buildExpressionParser optable basic
    where
        basic = fmap GivenValue (integer objc)
            <|> fmap GivenValue multiCharConstant
            <|> fmap TooComplicatedValue
                     (many1 (satisfy (\x -> x /= ';' && x /= '}')))
        optable = [ [Infix shiftLeft AssocLeft] ]
        
        shiftLeft = op "<<" (flip $ flip shiftL . fromIntegral)
        
        op str f = reservedOp objc str >> return (opFun f)
        opFun f (GivenValue x) (GivenValue y) = GivenValue $ f x y
        opFun f v@(TooComplicatedValue _) _ = v
        opFun f _ v@(TooComplicatedValue _) = v
        opFun f _ _ = TooComplicatedValue "..."
        
enum_type =
    do
        key <- reserved objc "enum"
        id <- identifier objc <|> return ""
        body <- braces objc enum_body <|> return []
        return $ CTEnum id body
    where
        enum_body = commaSep objc enum_entry
        enum_entry = do
            id <- identifier objc
            val <- (do
                    symbol objc "="
                    const_int_expr
                ) <|> return NextValue
            return (id,val)
    
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
            semi objc
            return $ Just $ ExternFun (Selector n t args False)
    <|> do
            semi objc
            return $ Just $ ExternVar t n
    where
        argument = do t <- ctype
                      optional (identifier objc)
                      return t
                   
        
extern_keyword =
        reserved objc "extern"
    <|> reserved objc "FOUNDATION_EXPORT" -- N.B. "Export" vs. "Extern".
    <|> reserved objc "APPKIT_EXTERN"

skipParens = parens objc (skipMany (
    (satisfy (\x -> x /= '(' && x /= ')') >> return ())
    <|> skipParens
    ))

skipBlock = braces objc (skipMany (
    (satisfy (\x -> x /= '{' && x /= '}') >> return ())
    <|> skipBlock
    ))
