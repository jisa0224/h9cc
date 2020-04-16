import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then putStr $ compileCtoASM $ head args
        else hPutStrLn stderr "參數數量錯誤！"

compileCtoASM :: String -> String
compileCtoASM input =
    ".intel_syntax noprefix\n\
    \.global main\n\
    \main:\n" ++
    "  mov rax, " ++ takeWhile isDigit input ++ "\n" ++
    (generateRemainingAddSubCode $ dropWhile isDigit input) ++
    "  ret\n"

-- WARNING: Cannot handle space between operator and integer literal
generateRemainingAddSubCode :: String -> String
generateRemainingAddSubCode "" = ""
generateRemainingAddSubCode addSubCode
    | "+" `isPrefixOf` addSubCode =
        let (term, remaining) = span isDigit $ fromJust $ stripPrefix "+" addSubCode
        in "  add rax, " ++ term ++ "\n" ++ generateRemainingAddSubCode remaining
    | "-" `isPrefixOf` addSubCode =
        let (term, remaining) = span isDigit $ fromJust $ stripPrefix "-" addSubCode
        in "  sub rax, " ++ term ++ "\n" ++ generateRemainingAddSubCode remaining
    | otherwise =
        error $ "Cannot parse \"" ++ addSubCode ++ "\""