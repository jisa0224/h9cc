import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, stripPrefix, elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
       then putStr $ compileCtoASM $ head args
       else hPutStrLn stderr "參數數量錯誤！"

compileCtoASM :: String -> String
compileCtoASM input = 
    let tokens = tokenize input
     in unlines [".intel_syntax noprefix",
                 ".global main",
                 "main:",
                 if (not . null) tokens && (isIntegerLiteral $ head tokens)
                    then "  mov rax, " ++ (show $ (\(IntegerLiteral x) -> x) $ head tokens)
                    else error "必須以數字開頭！",
                 generateASMCodeFromTokens $ tail tokens,
                 "  ret"]

generateASMCodeFromTokens :: [Token] -> String
generateASMCodeFromTokens [] = ""
generateASMCodeFromTokens (Operator op:IntegerLiteral num:ts) = 
    case op of "+" -> "  add rax, " ++ (show num) ++ "\n"
               "-" -> "  sub rax, " ++ (show num) ++ "\n"
               _ -> error ("Unknown operator \"" ++ op ++ "\"")
    ++ generateASMCodeFromTokens ts
generateASMCodeFromTokens ts = error $ "Cannot generate code from token: " ++ (show $ take 5 ts) ++ " ..."


-- Tokenizer (Lexical Analysis): String -> Tokens
data Token = IntegerLiteral Int
           | Operator String
           deriving (Show, Eq)

isIntegerLiteral :: Token -> Bool
isIntegerLiteral (IntegerLiteral _) = True
isIntegerLiteral _ = False

tokenize :: String -> [Token]
tokenize "" = []
tokenize input
    -- WARNING: isSpace '\n' == True, remember to fix this when handling multi-line source code
    -- WARNING: this will not work for string literals
    | isSpace $ head input = tokenize $ dropWhile isSpace input
    | isDigit $ head input = (IntegerLiteral $ (read (takeWhile isDigit input) :: Int))
                             : (tokenize $ dropWhile isDigit input)
    | any (`isPrefixOf` input) operatorList =
        let op = head $ filter (`isPrefixOf` input) operatorList
         in (Operator op) : (tokenize $ fromJust $ stripPrefix op input)
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
          | NodeAdd Node Node
          | NodeSub Node Node
          | NodeMul Node Node
          | NodeDiv Node Node
          deriving (Show)

parse :: [Token] -> Node
parse tokens = 
    let (ast, remaining) = expr tokens
     in if null remaining then ast else error $ "Cannot parse tokens: " ++ (show remaining)

-- because + and - are left-associated, and Haskell use recursion instead of iteration,
-- we have to parse from right to left, so the left side will at deeper place in AST
-- example: (1 + 2 + 3 + 4) -> ((1 + 2 + 3) + 4) -> (((1 + 2) + 3) + 4) -> ((((1) + 2) + 3) + 4)
--                              ^lhs          ^rhs
expr :: [Token] -> (Node, [Token])
expr ts =
    let lastOperatorIndex = elemLastIndex True $ map (`elem` [Operator "+", Operator "-"]) ts
     in if lastOperatorIndex == Nothing
           then mul ts
           else 
               let (lhs, lremaining) = expr $ take (fromJust lastOperatorIndex) ts
                   op = ts !! (fromJust lastOperatorIndex)
                   (rhs, rremaining) = mul $ drop ((fromJust lastOperatorIndex) + 1) ts
                in if null lremaining
                      then case op of (Operator "+") -> (NodeAdd lhs rhs, rremaining)
                                      (Operator "-") -> (NodeSub lhs rhs, rremaining)
                      else error $ "Cannot parse: " ++ (show lremaining)

mul :: [Token] -> (Node, [Token])
mul ts =
    let lastOperatorIndex = elemLastIndex True $ map (`elem` [Operator "*", Operator "/"]) ts
     in if lastOperatorIndex == Nothing
           then term ts
           else 
               let (lhs, lremaining) = mul $ take (fromJust lastOperatorIndex) ts
                   op = ts !! (fromJust lastOperatorIndex)
                   (rhs, rremaining) = term $ drop ((fromJust lastOperatorIndex) + 1) ts
                in if null lremaining
                      then case op of (Operator "*") -> (NodeMul lhs rhs, rremaining)
                                      (Operator "/") -> (NodeDiv lhs rhs, rremaining)
                      else error $ "Cannot parse: " ++ (show lremaining)

term :: [Token] -> (Node, [Token])
term (IntegerLiteral x:ts) = (NodeIntegerLiteral x, ts)
term (Operator "(":ts) =
    let (exprInParenthesis, remaining) = expr ts
     in if head remaining == Operator ")"
           then (exprInParenthesis, tail remaining)
           else error "Cannot find \")\""
term ts = error $ show ts ++ " is not valid"

elemLastIndex :: Eq a => a -> [a] -> Maybe Int
elemLastIndex t xs = 
    let a = elemIndex t $ reverse xs
     in if a == Nothing then Nothing else Just (length xs - fromJust a - 1)