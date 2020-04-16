import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Char (isDigit, isSpace)
import Data.List (uncons, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then putStr $ compileCtoASM $ head args
        else hPutStrLn stderr "參數數量錯誤！"

compileCtoASM :: String -> String
compileCtoASM input = 
    let (firstToken, remainingTokens) = fromJust $ uncons $ tokenize input    -- `fromJust` is OK because there is always a `EOF` in the list
     in unlines [".intel_syntax noprefix",
                 ".global main",
                 "main:",
                 if isIntegerLiteral $ firstToken then "  mov rax, " ++ (show $ (\(IntegerLiteral x) -> x) firstToken) else errorWithoutStackTrace "必須以數字開頭！",
                 generateASMCodeFromTokens remainingTokens,
                 "  ret"]

generateASMCodeFromTokens :: [Token] -> String
generateASMCodeFromTokens [EOF] = ""
generateASMCodeFromTokens (Operator op:IntegerLiteral num:ts) = 
    case op of "+" -> "  add rax, " ++ (show num) ++ "\n"
               "-" -> "  sub rax, " ++ (show num) ++ "\n"
               _ -> errorWithoutStackTrace ("Unknown operator \"" ++ op ++ "\"")
    ++ generateASMCodeFromTokens ts
generateASMCodeFromTokens ts = errorWithoutStackTrace $ "Cannot generate code from token: " ++ (show $ take 5 ts) ++ " ..."

-- Tokenizer
data Token = EOF
           | Operator String
           | IntegerLiteral Int
           deriving (Show, Eq)

isIntegerLiteral :: Token -> Bool
isIntegerLiteral (IntegerLiteral _) = True
isIntegerLiteral _ = False

tokenize :: String -> [Token]
tokenize "" = [EOF]
tokenize input
    -- WARNING: isSpace '\n' == True, remember to fix this when handling multi-line source code
    -- WARNING: this will not work for string literals
    | isSpace $ head input = tokenize $ dropWhile isSpace input
    | isDigit $ head input = (IntegerLiteral $ (read (takeWhile isDigit input) :: Int))
                             : (tokenize $ dropWhile isDigit input)
    | any (`isPrefixOf` input) operatorList =
        let operator = head $ filter (`isPrefixOf` input) operatorList
         in (Operator operator) : (tokenize $ fromJust $ stripPrefix operator input)
    | otherwise = errorWithoutStackTrace $ "Cannot parse \"" ++ input ++ "\""
    where
        -- WARNING: multi-character operator must precede single-character operator
        -- WARNING: this only works when all operators are left-associated
        operatorList = ["+", "-"]