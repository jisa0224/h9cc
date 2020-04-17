-- Tokenizer (Lexical Analysis): String -> Tokens
module Tokenizer (
    Token (..),
    tokenize
) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

data Token = TokenIntegerLiteral Int
           | TokenOperator String
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize input
    -- WARNING: isSpace '\n' == True, remember to fix this when handling multi-line source code
    -- WARNING: this will not work for string literals
    | isSpace $ head input = tokenize $ dropWhile isSpace input
    | isDigit $ head input = (TokenIntegerLiteral $ (read (takeWhile isDigit input) :: Int))
                             : (tokenize $ dropWhile isDigit input)
    | any (`isPrefixOf` input) operatorList =
        let op = head $ filter (`isPrefixOf` input) operatorList
         in (TokenOperator op) : (tokenize $ fromJust $ stripPrefix op input)
    | otherwise = error $ "Tokenizer: cannot tokenize \"" ++ input ++ "\""
    where
        -- WARNING: multi-character operator must precede single-character operator
        -- WARNING: this only works when all operators are left-associated
        operatorList = ["==", "!=", "<=", "<", ">=", ">", "+", "-", "*", "/", "(", ")"]