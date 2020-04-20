-- Parser (Syntax analysis): Tokens -> Abstract Syntax Tree (AST)
module Parser (
    Node (..),
    parse
) where

import Tokenizer (Token (..))

-- program    = stmt*
-- stmt       = expr ";"
-- expr       = assign
-- assign     = equality ("=" assign)?                             right-associated
-- equality   = relational ("==" relational | "!=" relational)*    left-associated
-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*     left-associated
-- add        = mul ("+" mul | "-" mul)*                           left-associated
-- mul        = unary ("*" unary | "/" unary)*                     left-associated
-- unary      = ("+" | "-")? primary
-- primary    = num | ident | "(" expr ")"

data Node = NodeProgram {statements :: [Node],
                         localVarStackOffsetMap :: [(String, Int)],
                         stackBytesNeeded :: Int}
          | NodeIntegerLiteral Int
          | NodeLocalVariable String
          | NodeUnaryOperator String Node
          | NodeBinaryOperator String Node Node
          deriving (Show)

parse :: [Token] -> Node
parse [] = error "Parser: empty expression is not allowed"
parse ts = NodeProgram (program ts) [] 0

program :: [Token] -> [Node]
program [] = []
program ts =
    let (newStmt, remaining) = stmt ts
     in newStmt:(program remaining)

stmt :: [Token] -> (Node, [Token])
stmt ts =
    let (exprBeforeSemicolon, remaining) = expr ts
     in if (not . null) remaining && head remaining == TokenOperator ";"
           then (exprBeforeSemicolon, tail remaining)
           else error "Parser: missing \";\""

expr :: [Token] -> (Node, [Token])
expr ts = assign ts

-- assign is right-associated
assign :: [Token] -> (Node, [Token])
assign ts =
    let (lhs, lremaining) = equality ts
     in if (not . null) lremaining && head lremaining == TokenOperator "="
           then let (rhs, rremaining) = assign $ tail lremaining
                 in (NodeBinaryOperator "=" lhs rhs, rremaining)
           else (lhs, lremaining)

equality :: [Token] -> (Node, [Token])
equality ts = equality' $ relational ts
    where
        equality' :: (Node, [Token]) -> (Node, [Token])
        equality' (node, []) = (node, [])
        equality' (lhs, lremaining)
            | head lremaining `elem` [TokenOperator "==", TokenOperator "!="] = 
                let (rhs, rremaining) = relational $ tail lremaining
                    newLhs = ((\(TokenOperator x) -> NodeBinaryOperator x) $ head lremaining) lhs rhs
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
                    newLhs = ((\(TokenOperator x) -> NodeBinaryOperator x) $ head lremaining) lhs rhs
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
                    newLhs = ((\(TokenOperator x) -> NodeBinaryOperator x) $ head lremaining) lhs rhs
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
                    newLhs = ((\(TokenOperator x) -> NodeBinaryOperator x) $ head lremaining) lhs rhs
                in mul' (newLhs, rremaining)
            | otherwise = (lhs, lremaining)

unary :: [Token] -> (Node, [Token])
unary tokens@(TokenOperator op:ts) = case op of "+" -> primary ts
                                                "-" -> let (node, remaining) = primary ts
                                                        in (NodeBinaryOperator "-" (NodeIntegerLiteral 0) node, remaining)
                                                _ -> primary tokens
unary ts = primary ts

primary :: [Token] -> (Node, [Token])
primary (TokenIntegerLiteral num:ts) = (NodeIntegerLiteral num, ts)
primary (TokenIdentifier ident:ts) = (NodeLocalVariable ident, ts)
primary (TokenOperator "(":ts)
    | (TokenOperator ")") `elem` ts = 
        let (exprInParenthesis, _:exprAfterParenthesis) = span (/= TokenOperator ")") ts
            (node, remaining) = expr exprInParenthesis
         in if null remaining
               then (node, exprAfterParenthesis)
               else error $ "Parser: too much operands: " ++ show remaining
    | otherwise = error $ "Parser: missing \")\""
primary ts = error $ "Parser: cannot parse tokens: " ++ show ts