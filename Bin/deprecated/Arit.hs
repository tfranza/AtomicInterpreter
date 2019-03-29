module Bin.Arit where

import Bin.Core

data AExpr = Add  AExpr AExpr
           | Mul  AExpr AExpr
           | Sub  AExpr AExpr
           | Div  AExpr AExpr
           | Pow  AExpr AExpr
       --  | Max  AExpr AExpr
           | ALit Int
           deriving Show

evalAExpr :: AExpr -> Int
evalAExpr ex = case ex of
    Add  a b -> evalAExpr a + evalAExpr b
    Mul  a b -> evalAExpr a * evalAExpr b
    Sub  a b -> evalAExpr a - evalAExpr b
    Div  a b -> evalAExpr a `div` evalAExpr b
    Pow  a b -> evalAExpr a ^ evalAExpr b
--    Max  a b -> max (evalAExpr a) (evalAExpr b) 
    ALit a   -> a

aexpr :: Parser AExpr
aexpr = aterm `chainl1` addop

aterm :: Parser AExpr
aterm = afactor `chainl1` mulop

afactor :: Parser AExpr
afactor = (number >>= \n -> return (ALit n)) +++ parens aexpr

addop :: Parser (AExpr -> AExpr -> AExpr)
addop = (infixOp "+" Add) 
    +++ (infixOp "-" Sub)

mulop :: Parser (AExpr -> AExpr -> AExpr)
mulop = (infixOp "*" Mul) 
    +++ (infixOp "/" Div) 
    +++ (infixOp "^" Pow)
--    +++ (prefixOp "max " Max)

