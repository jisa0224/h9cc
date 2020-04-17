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
        ast = parse tokens
        asmCode = generateASMCodeFromAST ast
     in unlines [".intel_syntax noprefix",
                 ".global main",
                 "main:",
                 asmCode,
                 "  pop rax",
                 "  ret"]


-- Tokenizer (Lexical Analysis): String -> Tokens
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


-- Parser (Syntax analysis): Tokens -> Abstract Syntax Tree (AST)

-- expr       = equality
-- equality   = relational ("==" relational | "!=" relational)*
-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*
-- add        = mul ("+" mul | "-" mul)*
-- mul        = unary ("*" unary | "/" unary)*
-- unary      = ("+" | "-")? term
-- term       = num | "(" expr ")"

data Node = NodeIntegerLiteral Int
          | NodeOperator String Node Node
          deriving (Show)

parse :: [Token] -> Node
parse [] = error "Parser: empty expression is not allowed"
parse ts =
    let (ast, remaining) = expr ts
     in if null remaining
           then ast
           else error $ "Parser: too much operands: " ++ show remaining

expr :: [Token] -> (Node, [Token])
expr ts = equality ts

equality :: [Token] -> (Node, [Token])
equality ts = equality' $ relational ts
    where
        equality' :: (Node, [Token]) -> (Node, [Token])
        equality' (node, []) = (node, [])
        equality' (lhs, lremaining)
            | head lremaining `elem` [TokenOperator "==", TokenOperator "!="] = 
                let (rhs, rremaining) = relational $ tail lremaining
                    newLhs = ((\(TokenOperator x) -> NodeOperator x) $ head lremaining) lhs rhs
                in equality' (newLhs, rremaining)
            | otherwise = (lhs, lremaining)

relational :: [Token] -> (Node, [Token])
relational ts = relational' $ add ts
    where
        relational' :: (Node, [Token]) -> (Node, [Token])
        relational' (node, []) = (node, [])
        relational' (lhs, lremaining)
            | head lremaining `elem` [TokenOperator "<=", TokenOperator "<", TokenOperator ">=", TokenOperator ">"] = 
                let (rhs, rremaining) = add $ tail lremaining
                    newLhs = ((\(TokenOperator x) -> NodeOperator x) $ head lremaining) lhs rhs
                in relational' (newLhs, rremaining)
            | otherwise = (lhs, lremaining)

add :: [Token] -> (Node, [Token])
add ts = add' $ mul ts
    where
        add' :: (Node, [Token]) -> (Node, [Token])
        add' (node, []) = (node, [])
        add' (lhs, lremaining)
            | head lremaining `elem` [TokenOperator "+", TokenOperator "-"] = 
                let (rhs, rremaining) = mul $ tail lremaining
                    newLhs = ((\(TokenOperator x) -> NodeOperator x) $ head lremaining) lhs rhs
                in add' (newLhs, rremaining)
            | otherwise = (lhs, lremaining)

mul :: [Token] -> (Node, [Token])
mul ts = mul' $ unary ts
    where
        mul' :: (Node, [Token]) -> (Node, [Token])
        mul' (node, []) = (node, [])
        mul' (lhs, lremaining)
            | head lremaining `elem` [TokenOperator "*", TokenOperator "/"] = 
                let (rhs, rremaining) = unary $ tail lremaining
                    newLhs = ((\(TokenOperator x) -> NodeOperator x) $ head lremaining) lhs rhs
                in mul' (newLhs, rremaining)
            | otherwise = (lhs, lremaining)

unary :: [Token] -> (Node, [Token])
unary tokens@(TokenOperator op:ts) = case op of "+" -> term ts
                                                "-" -> let (node, remaining) = term ts
                                                        in (NodeOperator "-" (NodeIntegerLiteral 0) node, remaining)
                                                _ -> term tokens
unary ts = term ts

term :: [Token] -> (Node, [Token])
term (TokenIntegerLiteral num:ts) = (NodeIntegerLiteral num, ts)
term (TokenOperator "(":ts)
    | (TokenOperator ")") `elem` ts = 
        let (exprInParenthesis, _:exprAfterParenthesis) = span (/= TokenOperator ")") ts
            (node, remaining) = expr exprInParenthesis
         in if null remaining
               then (node, exprAfterParenthesis)
               else error $ "Parser: too much operands: " ++ show remaining
    | otherwise = error $ "Parser: cannot find \")\""
term ts = error $ "Parser: cannot parse tokens: " ++ show ts


-- Code generator: generate Assembly Code from AST
generateASMCodeFromAST :: Node -> String
generateASMCodeFromAST (NodeIntegerLiteral num) = "  push " ++ show num
generateASMCodeFromAST (NodeOperator op lhs rhs) =
    unlines [generateASMCodeFromAST lhs,
             generateASMCodeFromAST rhs,
             "  pop rdi",
             "  pop rax",
             case op of "==" -> "  cmp rax, rdi\n  sete al\n  movzb rax, al"
                        "!=" -> "  cmp rax, rdi\n  setne al\n  movzb rax, al"
                        "<=" -> "  cmp rax, rdi\n  setle al\n  movzb rax, al"
                        "<"  -> "  cmp rax, rdi\n  setl al\n  movzb rax, al"
                        ">=" -> "  cmp rax, rdi\n  setge al\n  movzb rax, al"
                        ">"  -> "  cmp rax, rdi\n  setg al\n  movzb rax, al"
                        "+"  -> "  add rax, rdi"
                        "-"  -> "  sub rax, rdi"
                        "*"  -> "  imul rax, rdi"
                        "/"  -> "  cqo\n  idiv rdi"
                        _ -> error $ "Code generator: unknown operator: " ++ op,
             "  push rax"]