module ExpressionParser

where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [ [op "*" (*) AssocLeft, op "/" div AssocLeft]
        , [op "+" (+) AssocLeft, op "-" (-) AssocLeft] ]
  where
    op s f assoc = Infix (do { string s; return f }) assoc

factor = do { char '('; x <- expr; char ')'; return x }
         <|> number
         <?> "simple expression"

number :: Parser Integer
number = do { ds <- many1 digit; return (read ds) } <?> "number"

