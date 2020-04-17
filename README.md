# h9cc

A C compiler written in Haskell for learning purpose

依據 [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook) 和其繁體中文翻譯版 [C編譯器入門～想懂低階系統從自幹編譯器開始～](https://koshizuow.gitbook.io/compilerbook/) 進行開發，主要目的是學習如何從零開始編寫一個編譯器。

原文的 9cc 編譯器採用 C 語言開發，我改用 Haskell 開發，因此叫做 **h**9cc。

1. 這篇教學文章很好的平衡理論和實作，在需要用到的時候才解說相關的知識，不會一開始就要掌握一大堆理論，結果動手時卻寫不出來。
2. 教科書和許多網路文章都會採用現成的工具（如 lex 和 yacc）來開發編譯器，或許是因為它們的重點是放在最佳化才這樣，但對我來說學習編譯器的重點是在詞法分析、語法分析和語意分析上，所以我必須要自己動手做。

## 使用

```
runghc -i./src ./src/Main.hs
```

或

```
ghc -i./src -o h9cc ./src/Main.hs
./h9cc
```

## 程式碼結構

```
Main.hs             -- 程式進入點
Tokenizer.hs        -- 詞法分析（Lexical analysis）
Parser.hs           -- 語法分析（Syntactic analysis）和語意分析（Semantic analysis）
CodeGenerator.hs    -- 生成組合語言
```