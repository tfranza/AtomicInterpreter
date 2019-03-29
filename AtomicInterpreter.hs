module AtomicInterpreter where

import Bin.Core
import Bin.Stmt
import Bin.Eval

-- runs the interpreter
main :: IO()
main = forever $ putStr "> " >> getLine >>= \code -> putStr $ showEnv $ runProgram $ runParser whileParser code

-- runs the parser
mainParser :: IO()
mainParser = forever $ putStr "> " >> getLine >>= \code -> putStr $ show $ runParser whileParser code