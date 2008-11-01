module Preprocessor( preprocess ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Expr

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

line = (try (whiteSpace cpp >> symbol cpp "#") >> directive) <|> fmap Text plainLine

directive = 
    (reserved cpp "if" >> expression >>= \e -> return $ If e)
    <|> (reserved cpp "elif" >> expression >>= \e -> return $ Elif e)
    <|> (reserved cpp "ifdef" >> definedMacroCondition >>= \e -> return $ If e)
    <|> (reserved cpp "ifndef" >> definedMacroCondition >>= \e -> return $ If (negateExpr e))
    <|> (reserved cpp "endif" >> return Endif)
    <|> (reserved cpp "else" >> return Else)    
    <|> (plainLine >>= \p -> return $ Text ("//#" ++ p))
    
definedMacroCondition = do
    macro <- identifier cpp
    return (get >>= return . maybe 0 (const 1) . Map.lookup macro)

negateExpr e = e >>= \x -> return (if x /= 0 then 0 else 1)
    
expression = try (buildExpressionParser optable basic) <|> return (return 0)
    where
        basic :: Parsec String () Expr    
        basic = do i <- integer cpp
                   return (return i)
            <|> do reserved cpp "defined"
                   parens cpp definedMacroCondition
            <|> do reserved cpp "OS_API_VERSION"
                   parens cpp (identifier cpp >> comma cpp >> identifier cpp)
                   return (return 1)
            <|> do reservedOp cpp "!"
                   x <- basic
                   return (x >>= return . (\xx -> if xx /= 0 then 0 else 1))
            <|> parens cpp expression
            <|> do x <- identifier cpp
                   return (get >>= return . maybe 0 id . Map.lookup x)
        
        optable = [ [Infix (op "*" (*)) AssocLeft,
                     Infix (op "/" div) AssocLeft],
                    [Infix (op "+" (+)) AssocLeft,
                     Infix (op "-" (-)) AssocLeft],
                    [Infix (bop "<" (<)) AssocLeft,
                     Infix (bop "<=" (<=)) AssocLeft,
                     Infix (bop "==" (==)) AssocLeft,
                     Infix (bop "!=" (/=)) AssocLeft,
                     Infix (bop ">=" (>=)) AssocLeft,
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
   --  oneOf "\n\r"
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
     ("MAC_OS_X_VERSION_10_5", 1050),
     ("__OBJC__", 1)
    ]

execute :: String -> [PPLine] -> String
execute filename xs = unlines $ evalState (exec xs []) macros where
    exec (If e : xs) state@( (_, False) : _ )
        = output "//#if" $ exec xs ((PPSIf False, False) : state)
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
    exec a@(_:_) b = error $ "Preprocessor error in file " ++ filename ++ " " ++ show (a,b)
    exec [] [] = return []
    exec [] s = error $ "Preprocessor error in file " ++ filename ++ " " ++ show s

    output t more = do moreText <- more
                       return (t : moreText)
                       
    
unblockComments ('/' : '*' : xs) = "/*" ++ handleComment xs
    where handleComment ('*' : '/' : xs) = "*/" ++ unblockComments xs
          handleComment ('\n' : xs) = "*/\n/*" ++ handleComment xs
          handleComment (c : xs) = c : handleComment xs
          handleComment [] = []
unblockComments (c : xs) = c : unblockComments xs
unblockComments [] = "\n"

parseDirectives = map (\l -> case parse line "" l of
                                Left e -> Text $ l ++ "// " ++ show (show e)
                                Right x -> x) . handleBackslashes . lines . unblockComments        
        
handleBackslashes [] = []
handleBackslashes (l : ls)
    | null l = [] : handleBackslashes ls
    | last l == '\\' = case handleBackslashes ls of
                            (l2 : ls') -> (l ++ '\n' : l2) : ls'
                            ls' -> ls'
    | otherwise = l : handleBackslashes ls

preprocess :: String -> String -> String        
preprocess fn f = execute fn $ parseDirectives f

