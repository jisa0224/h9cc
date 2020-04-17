import System.Environment (getArgs)

import Tokenizer (tokenize)
import Parser (parse)
import CodeGenerator (generateASMCode)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
       then putStr $ compileCtoASM $ head args
       else error "參數數量錯誤！"

compileCtoASM :: String -> String
compileCtoASM input = generateASMCode $ parse $ tokenize input