-- Analyzer (Semantic analysis): Abstract Syntax Tree (AST) -> Abstract Syntax Tree (AST) with semantic message
module Analyzer (
    analyze
) where

import Data.List (nub)
import Data.Maybe (fromJust)

import Parser (Node (..))

analyze :: Node -> (Node, Int)    -- Bytes need for stack
analyze prog@(NodeProgram stmts) =
    let varAddrMap = allocateVariableOnStack $ getVariableList prog
        stackBytesNeeded = if null varAddrMap then 0 else snd $ head varAddrMap
        progAllocated = fillVariableAddress varAddrMap prog
        operatorAssignChecked = checkOperatorAssign progAllocated    -- program exit here if didn't pass the check
     in (progAllocated, stackBytesNeeded)


-- Allocate memory for variables
getVariableList :: Node -> [String]
getVariableList (NodeProgram stmts) = nub $ concatMap getVariableList stmts
getVariableList (NodeIntegerLiteral _) = []
getVariableList (NodeLocalVariable name _) = [name]
getVariableList (NodeUnaryOperator _ node) = getVariableList node
getVariableList (NodeBinaryOperator _ lnode rnode) = (getVariableList lnode) ++ (getVariableList rnode)

allocateVariableOnStack :: [String] -> [(String, Int)]
allocateVariableOnStack [] = []
allocateVariableOnStack (v:vs) = (v, (length vs + 1) * 8) : allocateVariableOnStack vs

fillVariableAddress :: [(String, Int)] -> Node -> Node
fillVariableAddress varAddrMap (NodeProgram stmts) = NodeProgram $ map (fillVariableAddress varAddrMap) stmts
fillVariableAddress varAddrMap (NodeLocalVariable name _) = NodeLocalVariable name $ fromJust $ lookup name varAddrMap
fillVariableAddress varAddrMap (NodeUnaryOperator op value) = NodeUnaryOperator op $ fillVariableAddress varAddrMap value
fillVariableAddress varAddrMap (NodeBinaryOperator op lhs rhs) = NodeBinaryOperator op (fillVariableAddress varAddrMap lhs) (fillVariableAddress varAddrMap rhs)
fillVariableAddress _ node = node


-- Check if all assign ("=") statement is valid (lhs can be assigned)
checkOperatorAssign :: Node -> Bool
checkOperatorAssign (NodeProgram stmts) = all checkOperatorAssign stmts
checkOperatorAssign (NodeUnaryOperator _ value) = checkOperatorAssign value
checkOperatorAssign (NodeBinaryOperator "=" lhs rhs) = isAssignable lhs && checkOperatorAssign rhs
checkOperatorAssign (NodeBinaryOperator _ lhs rhs) = checkOperatorAssign lhs && checkOperatorAssign rhs
checkOperatorAssign _ = True

isAssignable :: Node -> Bool
isAssignable (NodeLocalVariable _ _) = True
isAssignable node = error $ "Analyzer: " ++ show node ++ " lhs cannot be assigned"