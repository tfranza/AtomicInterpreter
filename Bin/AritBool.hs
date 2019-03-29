module Bin.AritBool where

import Bin.Core

-- declaration of Arithmetic and Boolean Expressions Types
data Expr = Var  String 
          | Add  Expr Expr
          | Mul  Expr Expr
          | Sub  Expr Expr
          | Div  Expr Expr
          | Pow  Expr Expr
          | ALit Int
          | And  Expr Expr
          | Or   Expr Expr
          | Nand Expr Expr
          | Nor  Expr Expr
          | BLit Bool
          | Get  Expr Expr
          | Let  Expr Expr
          | Gt   Expr Expr
          | Lt   Expr Expr
          | Eq   Expr Expr
          | Not  Expr
          deriving (Eq, Show)

-- elementary unit in order to do operations
factor :: Parser Expr
factor = (number >>= \n -> return (ALit n)) 
     +++ (identifier >>= \s -> return (Var s))
     +++ (btrue >>= \b -> return (BLit b))
     +++ (bfalse >>= \b -> return (BLit b)) 
     +++ parens aexpr 
     +++ parens bexpr

-- operations of sum and subtraction (sum with opposite number)
addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) 
    +++ (infixOp "-" Sub)

-- operations of product, division (product with reciprocal) and power
mulop :: Parser (Expr -> Expr -> Expr)
mulop = (infixOp "*" Mul) 
    +++ (infixOp "/" Div) 
    +++ (infixOp "^" Pow)

-- combination of arithmetic factors using sums and products with priority to products
aexpr :: Parser Expr
aexpr = (factor `chainl1` mulop) `chainl1` addop

-- binary operations between booleans
boolop :: Parser (Expr -> Expr -> Expr)
boolop = (infixOp "and" And) 
     +++ (infixOp "or" Or) 
     +++ (infixOp "nand" Nand)
     +++ (infixOp "nor" Nor)
     +++ (infixOp ">=" Get)
     +++ (infixOp "<=" Let)
     +++ (infixOp ">" Gt)
     +++ (infixOp "<" Lt)
     +++ (infixOp "=" Eq)

-- unary operations between booleans 
boolop1 :: Parser (Expr -> Expr)
boolop1 = (prefixOp "not" Not)

-- combination of boolean factors using unary or binary operations 
bexpr :: Parser Expr
bexpr = (factor `chain1` boolop1) +++ (factor `chainl1` boolop)
