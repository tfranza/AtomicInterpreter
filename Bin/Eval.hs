module Bin.Eval where

import qualified Data.Map as Map

import Bin.AritBool
import Bin.Stmt

-- environment declaration
type Env = Map.Map String Int
    
-- evaluates arithmetic expressions
evalA :: (Env, Expr) -> Int
evalA (env, e) = case e of
    ALit a     -> a
    Var  s     -> env Map.! s -- precondition: s is in the environment
    Add  a1 a2 -> evalA (env, a1) + evalA(env, a2)
    Mul  a1 a2 -> evalA (env, a1) * evalA(env, a2)
    Sub  a1 a2 -> evalA (env, a1) - evalA(env, a2)
    Div  a1 a2 -> evalA (env, a1) `div` evalA(env, a2)
    Pow  a1 a2 -> evalA (env, a1) ^ evalA(env, a2)

-- evaluates boolean expressions
evalB :: (Env, Expr) -> Bool
evalB (env, e) = case e of
    BLit b     ->       b
    And  b1 b2 ->       evalB (env, b1) && evalB (env, b2)
    Or   b1 b2 ->       evalB (env, b1) || evalB (env, b2)
    Nand b1 b2 -> not ( evalB (env, b1) && evalB (env, b2) )
    Nor  b1 b2 -> not ( evalB (env, b1) || evalB (env, b2) )
    Get  a1 a2 ->       evalA (env, a1) >= evalA (env, a2)
    Let  a1 a2 ->       evalA (env, a1) <= evalA (env, a2)
    Gt   a1 a2 ->       evalA (env, a1) >  evalA (env, a2)
    Lt   a1 a2 ->       evalA (env, a1) <  evalA (env, a2)
    Eq   a1 a2 ->       evalA (env, a1) == evalA (env, a2)
    Not  b     -> not ( evalB (env, b) )

-- evaluates command expressions
evalC :: (Env, Stmt) -> Env
evalC (env, s) = case s of
    Skip          -> env
    Assign var a  -> Map.insert var (evalA (env, a)) env 
    Seq l         -> case l of
        (l:[])        -> evalC (env, l)
        (l:ls)        -> evalC (evalC (env, l), Seq ls)
    If b s1 s2    -> case (evalB (env, b)) of 
        True          -> evalC (env, s1) 
        False         -> evalC (env, s2)
    While b block -> case evalB (env, b) of
        True          -> evalC( evalC (env, block), While b block )
        False         -> env

-- shows the stored variables and their values in the environment 
showEnv :: Env -> String
showEnv env = let 
    f = \(k,v) -> k ++ " " ++ show v
    in unlines $ map f $ Map.toList env

-- runs evaluation on commands
runProgram :: Stmt -> Env
runProgram s = evalC (Map.empty, s)
