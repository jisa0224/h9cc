-- Analyzer (Semantic analysis): Abstract Syntax Tree (AST) -> Abstract Syntax Tree (AST) with semantic message
module Analyzer (
    analyze
) where

import Data.List (nub)

import Parser (Node (..))

-- `analyze` uses pipeline model by chaining a set of analysis
analyze :: Node -> Node
analyze prog = checkOperatorAssign $ allocateLocalVariableOnStack prog

allocateLocalVariableOnStack :: Node -> Node
allocateLocalVariableOnStack (NodeProgram stmts _ _) =
    let localVarList = nub $ concatMap getLocalVarList  stmts
        localVarOffsetMap = setLocalVarOffset localVarList
        stackBytesRequire = if null localVarOffsetMap then 0 else snd $ head localVarOffsetMap
     in NodeProgram stmts localVarOffsetMap stackBytesRequire
     where
         getLocalVarList :: Node -> [String]
         getLocalVarList (NodeIntegerLiteral _) = []
         getLocalVarList (NodeLocalVariable name) = [name]
         getLocalVarList (NodeUnaryOperator _ node) = getLocalVarList node
         getLocalVarList (NodeBinaryOperator _ lnode rnode) = (getLocalVarList lnode) ++ (getLocalVarList rnode)
         
         setLocalVarOffset :: [String] -> [(String, Int)]
         setLocalVarOffset [] = []
         setLocalVarOffset (v:vs) = (v, (length vs + 1) * 8) : setLocalVarOffset vs

checkOperatorAssign :: Node -> Node
checkOperatorAssign prog =
    if isAssignValid prog then prog else error "Analyzer: never reaches here"
    where
        isAssignValid :: Node -> Bool
        isAssignValid (NodeProgram stmts _ _) = all isAssignValid stmts
        isAssignValid (NodeUnaryOperator _ value) = isAssignValid value
        isAssignValid (NodeBinaryOperator "=" lhs rhs) = isMemoryAddress lhs && isAssignValid rhs
        isAssignValid (NodeBinaryOperator _ lhs rhs) = isAssignValid lhs && isAssignValid rhs
        isAssignValid _ = True
        
        isMemoryAddress :: Node -> Bool
        isMemoryAddress (NodeLocalVariable _) = True
        isMemoryAddress node = error $ "Analyzer: " ++ show node ++ " lhs cannot be assigned"