module Bin.Bool where

import Bin.Arit
import Bin.Core

data BExpr = And  BExpr BExpr
           | Or   BExpr BExpr
           | Nand BExpr BExpr
           | Nor  BExpr BExpr
           | Eq   BExpr BExpr
           | BLit Bool
           deriving Show

evalBExpr :: BExpr -> Bool
evalBExpr ex = case ex of
    And  a b -> evalBExpr a && evalBExpr b
    Or   a b -> evalBExpr a || evalBExpr b
    Nand a b -> not (evalBExpr a && evalBExpr b)
    Nor  a b -> not (evalBExpr a || evalBExpr b)
    Eq   a b -> (evalBExpr a) == (evalBExpr b)
    BLit   b -> b

bexpr :: Parser BExpr
bexpr = bfactor `chainl1` bop

--bterm :: Parser BExpr
--bterm = afactor `chainl1` aop

bfactor :: Parser BExpr
bfactor = (btrue >>= \b -> return (BLit b)) +++ (bfalse >>= \b -> return (BLit b)) +++ parens bexpr

bop :: Parser (BExpr -> BExpr -> BExpr)
bop = (infixOp "and" And) 
  +++ (infixOp "or" Or) 
  +++ (infixOp "nand" Nand)
  +++ (infixOp "nor" Nor)
  +++ (infixOp "=" Eq)
--  +++ (Not "not")

--aop :: Parser (AExpr -> AExpr -> BExpr)
--aop = (infixOp ">=" Get) 
--  +++ (infixOp "<=" Let)

