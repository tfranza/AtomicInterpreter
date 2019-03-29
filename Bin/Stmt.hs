module Bin.Stmt where

import Bin.AritBool
import Bin.Core

-- statements declaration types
data Stmt = Seq [Stmt]
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Skip
          deriving (Show)

-- useful for single or multiple recursive statements
statement :: Parser Stmt
statement =  parens statement +++ sequenceOfStmt

-- declaration of single kind of statement
statement' :: Parser Stmt
statement' =  skipStmt +++ assignStmt +++ ifStmt +++ whileStmt 

-- useful to identify more statements separated by colon
sequenceOfStmt :: Parser Stmt
sequenceOfStmt = sepby1 statement' (reserved ";") >>= \list -> (return $ if length list == 1 then head list else Seq list)

-- declaration of skip operation structure
skipStmt :: Parser Stmt
skipStmt = space >> reserved "skip" >> space >> return Skip

-- declaration of assign operation structure
assignStmt :: Parser Stmt
assignStmt = space >> identifier >>= \var -> space >> reserved ":=" >> space >> aexpr >>= \expr -> space >> (return $ Assign var expr)

-- declaration of if operation structure
ifStmt :: Parser Stmt
ifStmt = space >> reserved "if" >> space >> bexpr >>= \cond -> space >> reserved "then" >> space >> statement >>= \stmt1 -> space >> reserved "else" >> space >> statement >>= \stmt2 -> space >> (return $ If cond stmt1 stmt2)

-- declaration of while operation structure
whileStmt :: Parser Stmt
whileStmt = space >> reserved "while" >> space >> bexpr >>= \cond -> space >> reserved "do" >> space >> statement >>= \stmt -> space >> (return $ While cond stmt)

-- general parser 
whileParser :: Parser Stmt
whileParser = space >> statement
