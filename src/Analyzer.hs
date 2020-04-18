-- Analyzer (Semantic analysis)
module Analyzer (
    getVariableList,
    allocateVariableOnStack
) where

import Data.List (nub)

import Parser (Node (..))

getVariableList :: Node -> [String]
getVariableList (NodeProgram stmts) = nub $ concatMap getVariableList stmts
getVariableList (NodeIntegerLiteral _) = []
getVariableList (NodeVariable name) = [name]
getVariableList (NodeUnaryOperator _ node) = getVariableList node
getVariableList (NodeBinaryOperator _ lnode rnode) = (getVariableList lnode) ++ (getVariableList rnode)


allocateVariableOnStack :: [String] -> [(String, Int)]
allocateVariableOnStack [] = []
allocateVariableOnStack (v:vs) = (v, (length vs + 1) * 8) : allocateVariableOnStack vs