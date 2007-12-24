module Preprocessor( preprocess ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Text.ParserCombinators.Parsec.Expr

import Control.Monad.State as StateM

import qualified Data.Map as Map


cppDef = emptyDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = False
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> char '_'
    , reservedNames  = ["define","undef","include","import","if","ifdef",
                        "ifndef", "elif", "endif", "defined"]
    , caseSensitive  = True
    }

cpp :: TokenParser ()
cpp = makeTokenParser cppDef


type Expr = StateM.State (Map.Map String Integer) Integer
data PPLine = Text String | If Expr | Else | Endif | Elif Expr

instance Show PPLine where
    show (Text s) = "Text " ++ show s
    show (If _) = "If"
    show Else = "Else"
    show Endif = "Endif"
    show (Elif _) = "Elif"

preprocessor = 
    many $
    (symbol cpp "#" >> preprocessorLine) <|> fmap Text plainLine
    
preprocessorLine = 
    (reserved cpp "if" >> expression >>= \e -> return $ If e)
    <|> (reserved cpp "elif" >> expression >>= \e -> return $ Elif e)
    <|> (reserved cpp "ifdef" >> definedMacroCondition >>= \e -> return $ If e)
    <|> (reserved cpp "ifndef" >> definedMacroCondition >>= \e -> return $ If (negateExpr e))
    <|> (reserved cpp "endif" >> return Endif)
    <|> (reserved cpp "else" >> return Else)    
    <|> (plainLine >>= \p -> return $ Text ("//# " ++ p))
    
definedMacroCondition = do
    macro <- identifier cpp
    return (get >>= return . maybe 0 (const 1) . Map.lookup macro)

negateExpr e = e >>= \x -> return (if x /= 0 then 0 else 1)
    
expression = buildExpressionParser optable basic
    where
        basic :: CharParser () Expr    
        basic = do i <- integer cpp
                   return (return i)
            <|> do reserved cpp "defined"
                   parens cpp definedMacroCondition
            <|> do reservedOp cpp "!"
                   x <- basic
                   return (x >>= return . (\xx -> if xx /= 0 then 0 else 1))
            <|> parens cpp expression
            <|> do x <- identifier cpp
                   return (get >>= return . maybe 0 id . Map.lookup x)
        
        optable = [ [Infix (bop "<" (<)) AssocLeft,
                     Infix (bop "<=" (<=)) AssocLeft,
                     Infix (bop "==" (==)) AssocLeft,
                     Infix (bop "!=" (/=)) AssocLeft,
                     Infix (bop ">=" (<=)) AssocLeft,
                     Infix (bop ">" (>)) AssocLeft],
                    [Infix (bbop "||" (||)) AssocLeft,
                     Infix (bbop "&&" (&&)) AssocLeft]
                   ]
        
        op str f = reservedOp cpp str >> return (opFun f)
        opFun f a b = do aa <- a
                         bb <- b
                         return (f aa bb)

        bop str f = op str (\a b -> if (f a b) then 1 else 0)
        bbop str f = bop str (\a b -> f (a/=0) (b/=0))

plainLine = do
    cs <- many (noneOf "\n\r")
    oneOf "\n\r"
    return cs

data PPState = PPSIf Bool | PPSElse
    deriving( Show)

macros = Map.fromList
    [("MAC_OS_X_VERSION_MAX_ALLOWED", 1050),
     ("MAC_OS_X_VERSION_10_0", 1000),
     ("MAC_OS_X_VERSION_10_1", 1010),
     ("MAC_OS_X_VERSION_10_2", 1020),
     ("MAC_OS_X_VERSION_10_3", 1030),
     ("MAC_OS_X_VERSION_10_4", 1040),
     ("MAC_OS_X_VERSION_10_5", 1050)
    ]

execute :: [PPLine] -> String
execute xs = unlines $ evalState (exec xs []) macros where
    exec (If e : xs) state@( (_, False) : _ )
        = output "//#if" $ exec xs ((PPSIf False, False) : state)
--    exec (Elif e : xs) state@( (PPSIf False, False) : (_, False) : _ )
--        = output "//#elif" $ exec xs state
--    exec (Else : xs) state@( (_, False) : _ )
--        = output "//#else" $ exec xs ((PPSElse, False) : state)
    exec (Text t : xs) state@( (_, False) : _ )
        = output ("//T " ++ t) $ exec xs state
    exec (Endif : xs) (_ : state)
        = output "//#endif" $ exec xs state
    exec (If e : xs) state = do
        condition <- e
        if condition /= 0
            then output "//#if 1" $ exec xs ((PPSIf False, True) : state)
            else output "//#if 0" $ exec xs ((PPSIf True, False) : state)
    exec (Elif e : xs) ((PPSIf False, _) : state)
        = output "//#elif" $ exec xs ((PPSIf False, False) : state)
    exec (Elif e : xs) ((PPSIf True, _) : state) = do
        condition <- e
        if condition /= 0
            then output "//#elif 1" $ exec xs ((PPSIf False, True) : state)
            else output "//#elif 0" $ exec xs ((PPSIf True, False) : state)
    exec (Else : xs) ((PPSIf b, _) : state)
        = output "//#else" $ exec xs ((PPSElse, b) : state)
    exec (Text t : xs) state
        = output t $ exec xs state
    exec a@(_:_) b = error $ show (a,b)
    exec [] [] = return []
    exec [] s = error (show $ s)

    output t more = do moreText <- more
                       return (t : moreText)
                       
    
test = putStrLn $ either show execute $ parse preprocessor "" 
   "#include <foo>\n\
    \blah\n\
    \foo bar\n\
    \#if 1\n\
    \baz\n\
    \#else\n\
    \quux\n\
    \#endif\n"

test2 fn = do
    f <- readFile $ "/System/Library/Frameworks/Foundation.framework/Versions/C/Headers/" ++ fn
    putStrLn $ either show execute $
        parse preprocessor fn f
        

test3 fn = do
    f <- readFile $ "/System/Library/Frameworks/Foundation.framework/Versions/C/Headers/" ++ fn
    -- putStrLn $ 
    putStrLn fn
    print $ length $ either show execute $
        parse preprocessor f f
        
preprocess fn f = either (\x -> error $ "Preprocessor error:" ++ show x) 
                         execute
                         $ parse preprocessor fn $ (++"\n") $ f
