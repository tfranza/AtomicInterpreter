module Bin.Utils where

-- recognizes different kind of spaces
isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\n' = True
isSpace '\t' = True
isSpace _    = False

-- recognizes different kind of digits
isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _   = False

-- recognizes different kind of boolean
isBool :: String -> Bool
isBool "True"  = True
isBool "False" = True
isBool _       = False

-- recognizes lower chars
isLower :: Char -> Bool
isLower 'a' = True
isLower 'b' = True
isLower 'c' = True
isLower 'd' = True
isLower 'e' = True
isLower 'f' = True
isLower 'g' = True
isLower 'h' = True
isLower 'i' = True
isLower 'j' = True
isLower 'k' = True
isLower 'l' = True
isLower 'm' = True
isLower 'n' = True
isLower 'o' = True
isLower 'p' = True
isLower 'q' = True
isLower 'r' = True
isLower 's' = True
isLower 't' = True
isLower 'u' = True
isLower 'v' = True
isLower 'w' = True
isLower 'x' = True
isLower 'y' = True
isLower 'z' = True
isLower _   = False

-- recognizes upper chars
isUpper :: Char -> Bool
isUpper 'A' = True
isUpper 'B' = True
isUpper 'C' = True
isUpper 'D' = True
isUpper 'E' = True
isUpper 'F' = True
isUpper 'G' = True
isUpper 'H' = True
isUpper 'I' = True
isUpper 'J' = True
isUpper 'K' = True
isUpper 'L' = True
isUpper 'M' = True
isUpper 'N' = True
isUpper 'O' = True
isUpper 'P' = True
isUpper 'Q' = True
isUpper 'R' = True
isUpper 'S' = True
isUpper 'T' = True
isUpper 'U' = True
isUpper 'W' = True
isUpper 'X' = True
isUpper 'Y' = True
isUpper 'Z' = True
isUpper _   = False

-- recognizes alphanumeric
isAlphanum :: Char -> Bool
isAlphanum c = isDigit c || isLower c || isUpper c

-- translates digits to integers
digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt  _  = -1
