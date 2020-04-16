import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

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
    "  mov rax, " ++ input ++ "\n" ++
    "  ret\n"