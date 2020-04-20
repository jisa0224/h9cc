-- Code generator: Abstract Syntax Tree (AST) -> Assembly Code
module CodeGenerator (
    generateASMCode
) where

import Data.Maybe (fromJust)

import Parser (Node (..))

generateASMCode :: Node -> String
generateASMCode prog@(NodeProgram stmts localVarOffsetMap stackBytesNeeded) = 
    unlines [".intel_syntax noprefix",
             ".global main",
             "main:",
             "  push rbp",
             "  mov rbp, rsp",
             "  sub rsp, " ++ show stackBytesNeeded,
             generateASMCodeFromAST prog,
             "  mov rsp, rbp",
             "  pop rbp",
             "  ret"]
    where
        generateASMCodeFromAST :: Node -> String
        generateASMCodeFromAST (NodeProgram stmts _ _) =
            unlines $ map (\stmt -> generateASMCodeFromAST stmt ++ "\n  pop rax") stmts
        generateASMCodeFromAST (NodeIntegerLiteral num) = "  push " ++ show num
        generateASMCodeFromAST (NodeLocalVariable name) =
            unlines ["  mov rax, rbp",
                     "  sub rax, " ++ show (fromJust $ lookup name localVarOffsetMap),
                     "  mov rax, [rax]",
                     "  push rax"]
        generateASMCodeFromAST (NodeUnaryOperator op value) =
            case op of "return" -> unlines [generateASMCodeFromAST value,
                                            "  pop rax",
                                            "  mov rsp, rbp",
                                            "  pop rbp",
                                            "  ret"]
                       _ -> error $ "Code generator: unknown operator: " ++ op
        generateASMCodeFromAST (NodeBinaryOperator "=" (NodeLocalVariable name) rhs) =  -- WARNING: this only works if lhs is a variable
            unlines ["  mov rax, rbp",
                     "  sub rax, " ++ show (fromJust $ lookup name localVarOffsetMap),
                     "  push rax",
                     generateASMCodeFromAST rhs,
                     "  pop rdi",
                     "  pop rax",
                     "  mov [rax], rdi",
                     "  push rdi"]
        generateASMCodeFromAST (NodeBinaryOperator op lhs rhs) =
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