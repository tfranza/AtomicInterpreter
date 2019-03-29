module Bin.Core where

import Bin.Utils

-- definition of parser
newtype Parser a = Parser (String -> [(a,String)])

class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

-- elementary monadic operations
class (Alternative m, Monad m) => MonadPlus m where
   mzero :: m a
   mzero = empty

   mplus :: m a -> m a -> m a
   mplus = (<|>)

-- to abilitate the functor operator <$>
instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, b) | (a, b) <- p s]

-- to abilitate the applicative operator <|>
instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) = Parser $ \s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1]

-- instance of correlated class Monad Parser
instance Monad Parser where
  return = unit
  (>>=)  = bind
  
-- instance of correlated class Alternative Parser
instance Alternative Parser where
  empty = failure
  (<|>) = (+++)

-- injects a single pure value as result, without reading from the parse stream
unit :: a -> Parser a
unit a = Parser $ \s -> [(a,s)]

-- extracts a single character from the stream and returns it in a tuple containing itself and the rest of the stream
item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

-- Binds two parsers together: chains the parsers such that result of one parser is fed into a function that takes that result and produces a new parser 
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \orig -> concatMap (\(a, remain) -> parse (f a) remain) $ parse p orig

-- repeats the action infinitely
forever :: (Monad m) => m a -> m b
forever a = a >> forever a

-- returns null when a reading error occurs
failure :: Parser a
failure = Parser $ \cs -> []

-- gives the result of p xor q
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

-- parses a sequence of p separated by sep and returns the result in a list 
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = p >>= \a -> many (sep >> p) >>= \as -> return (a:as)

-- useful to recursively use a binary operator to left
chain1 :: Parser a -> Parser (a -> a) -> Parser a
p `chain1` op = op >>= \f -> p >>= \b -> return (f b)

-- useful to recursively use a ternary operator to left
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= \a -> rest a
  where rest a = (op >>= \f -> p >>= \b -> rest (f a b)) <|> return a

-- takes a predicate and returns a Parser that consumes the first character if that predicate is satisfied, or fails otherwise
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = item >>= \c -> 
         if predicate c
         then unit c -- returns a parser
         else failure

-- get zero or more token from the stream
many :: Parser a -> Parser [a]
many p = some p +++ return []

-- get one or more token from the stream
some :: Parser a -> Parser [a]
some p = p >>= \v -> many p >>= \vs -> return (v:vs)

-- recognizes digits
digit :: Parser Char
digit = satisfy isDigit

-- recognizes numbers
number :: Parser Int
number = read <$> (some $ satisfy isDigit)

-- recognizes True
btrue :: Parser Bool
btrue = string "True" >>= \s -> return $ read s

-- recognizes False
bfalse :: Parser Bool
bfalse = string "False" >>= \s -> return $ read s

-- recognizes spaces
space :: Parser String
space = many $ satisfy isSpace

-- recognizes char
char :: Char -> Parser Char
char c = satisfy (c == )

-- recognizes strings
string :: String -> Parser String
string "" = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

-- recognizes lower char
lower :: Parser Char
lower = satisfy isLower

-- recognizes upper char
upper :: Parser Char
upper = satisfy isUpper

-- recognizes alphanumeric
alphanum :: Parser Char
alphanum = satisfy isAlphanum

-- recognizes an identifier from the stream
identifier :: Parser String
identifier = lower >>= \c -> many alphanum >>= \cs -> return (c:cs)

-- recognizes a reserved token from the stream
reserved :: String -> Parser String
reserved s = string s >>= \a -> space >> return a

-- recognizes parenthesis construct from the stream
parens :: Parser a -> Parser a
parens m = space >> reserved "(" >> space >> m >>= \n -> space >> reserved ")" >> space >> return n

-- recognizes infix operators
infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = space >> reserved x >> space >> return f

-- recognizes prefix operators
prefixOp :: String -> (a -> a) -> Parser (a -> a)
prefixOp x f = space >> reserved x >> space >> return f

-- applies parser to string
parse :: Parser a -> String -> [(a,String)]
parse (Parser p) inp = p inp

-- runs the parser and returns error if there are problems during the parsing operation
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error ("Parser did not consume entire stream. \n Remaining stream: '" ++ rs ++ "' \n")
    _           -> error "Parser error."



