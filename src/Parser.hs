-- Parser (Syntax analysis): Tokens -> Abstract Syntax Tree (AST)
module Parser (
    Node (..),
    parse
) where

import Tokenizer (Token (..))

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