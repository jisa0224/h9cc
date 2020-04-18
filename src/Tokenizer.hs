-- Tokenizer (Lexical Analysis): String -> Tokens
module Tokenizer (
    Token (..),
    tokenize
) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

data Token = TokenIntegerLiteral Int
           | TokenIdentifier String
           | TokenOperator String
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize input
    -- Space outside string literal
    | isSpace $ head input = tokenize $ dropWhile isSpace input

    -- Integer literal
    | isDigit $ head input =
        let (num, remaining) = span isDigit input
         in if null remaining || head remaining `notElem` identInitialAllow
               then TokenIntegerLiteral (read num :: Int) : tokenize remaining
               else error $ "Tokenizer: \"" ++ num ++ (takeWhile (`elem` identNameAllow) remaining) ++ "\" identifier cannot start with number"
    
    -- Identifier
    | head input `elem` identInitialAllow =
        let (ident, remaining) = span (`elem` identNameAllow) input
         in TokenIdentifier ident : tokenize remaining
    
    -- Operator
    | any (`isPrefixOf` input) operatorList =
        -- Choose longest multi-character operator if more then one operator have the same initial
        -- eg: choose "<=" instead of "<", or it will be incorrectly seperated as "<" and "="
        let op = chooseLogenst $ filter (`isPrefixOf` input) operatorList
            remaining = fromJust $ stripPrefix op input
         in TokenOperator op : tokenize remaining
    
    -- Unknown
    | otherwise =
        error $ "Tokenizer: unknown operator \"" ++ (takeWhile (`notElem` (' ':identNameAllow)) input) ++ "\""
    
    where
        identInitialAllow = ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
        identNameAllow = identInitialAllow ++ ['0'..'9']
        operatorList = [";", "=", "==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", "(", ")"]
        chooseLogenst :: [String] -> String
        chooseLogenst [str] = str
        chooseLogenst (x:xs) = if (length x) >= (length $ chooseLogenst xs) then x else chooseLogenst xs