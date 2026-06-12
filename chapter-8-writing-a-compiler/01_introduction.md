## Introduction

A compiler turns one language into another. For our purposes, that means taking the rule-and-game definitions we expressed in CardLang (Chapter 7) and producing something a node can actually execute — bytecode, a contract, or an interpreter loop, depending on where the rules need to run.

This chapter walks through the parts of a compiler in the order you'd build one:

- Lexing — turning source text into tokens.
- Parsing — turning tokens into an abstract syntax tree.
- Semantic analysis — type-checking, name resolution, validation.
- Intermediate representation — what the optimiser actually works on.
- Code generation — the bytecode, target language, or interpreter input we emit.
- Runtime / VM — what executes the compiled output.

Each subsequent page covers one of these stages with worked examples drawn from CardLang. The point isn't to build a production-grade compiler in 200 pages — it's to make the *boundaries* between stages obvious, so when you're debugging a real-world DSL you know where the bug is hiding.
