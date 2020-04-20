-- Code generator: Abstract Syntax Tree (AST) -> Assembly Code
module CodeGenerator (
    generateASMCode
) where

import Parser (Node (..))

generateASMCode :: (Node, Int) -> String
generateASMCode (ast, stackBytesNeeded) = unlines
    [".intel_syntax noprefix",
     ".global main",
     "main:",
     "  push rbp",
     "  mov rbp, rsp",
     "  sub rsp, " ++ show stackBytesNeeded,
     generateASMCodeFromAST ast,
     "  mov rsp, rbp",
     "  pop rbp",
     "  ret"]

generateASMCodeFromAST :: Node -> String
generateASMCodeFromAST (NodeProgram stmts) =
    unlines $ map (\stmt -> generateASMCodeFromAST stmt ++ "\n  pop rax") stmts
generateASMCodeFromAST (NodeIntegerLiteral num) = "  push " ++ show num
generateASMCodeFromAST (NodeLocalVariable _ offset) =
    unlines ["  mov rax, rbp",
             "  sub rax, " ++ show offset,
             "  mov rax, [rax]",
             "  push rax"]
-- generateASMCodeFromAST (NodeUnaryOperator op value)
generateASMCodeFromAST (NodeBinaryOperator "=" (NodeLocalVariable _ offset) rhs) =  -- WARNING: this only works if lhs is a variable
    unlines ["  mov rax, rbp",
             "  sub rax, " ++ show offset,
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