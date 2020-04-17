import System.Environment (getArgs)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
       then putStr $ compileCtoASM $ head args
       else error "參數數量錯誤！"

compileCtoASM :: String -> String
compileCtoASM input = 
    let tokens = tokenize input
     in unlines [".intel_syntax noprefix",
                 ".global main",
                 "main:",
                 if (not . null) tokens && (isTokenIntegerLiteral $ head tokens)
                    then "  mov rax, " ++ (show $ (\(TokenIntegerLiteral x) -> x) $ head tokens)
                    else error "必須以數字開頭！",
                 generateASMCodeFromTokens $ tail tokens,
                 "  ret"]

generateASMCodeFromTokens :: [Token] -> String
generateASMCodeFromTokens [] = ""
generateASMCodeFromTokens (TokenOperator op:TokenIntegerLiteral num:ts) = 
    case op of "+" -> "  add rax, " ++ (show num) ++ "\n"
               "-" -> "  sub rax, " ++ (show num) ++ "\n"
               _ -> error ("Unknown operator \"" ++ op ++ "\"")
    ++ generateASMCodeFromTokens ts
generateASMCodeFromTokens ts = error $ "Cannot generate code from token: " ++ (show $ take 5 ts) ++ " ..."


-- Tokenizer (Lexical Analysis): String -> Tokens
data Token = TokenIntegerLiteral Int
           | TokenOperator String
           deriving (Show, Eq)

isTokenIntegerLiteral :: Token -> Bool
isTokenIntegerLiteral (TokenIntegerLiteral _) = True
isTokenIntegerLiteral _ = False

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
    | otherwise = error $ "Cannot parse \"" ++ input ++ "\""
    where
        -- WARNING: multi-character operator must precede single-character operator
        -- WARNING: this only works when all operators are left-associated
        operatorList = ["+", "-", "*", "/", "(", ")"]


-- Parser (Syntax analysis): Tokens -> Abstract Syntax Tree (AST)

-- expr = mul ("+" mul | "-"  mul)*
-- mul  = term ("*" term | "/" term)*
-- term = num | "(" expr ")"

data Node = NodeIntegerLiteral Int
          | NodeOperator String Node Node
          deriving (Show)