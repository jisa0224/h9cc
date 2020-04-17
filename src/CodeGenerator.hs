-- Code generator: Abstract Syntax Tree (AST) -> Assembly Code
module CodeGenerator (
    generateASMCode
) where

import Parser (Node (..))

generateASMCode :: Node -> String
generateASMCode ast = unlines
    [".intel_syntax noprefix",
     ".global main",
     "main:",
     generateASMCodeFromAST ast,
     "  pop rax",
     "  ret"]

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