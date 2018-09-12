# Untyped lambda calculus interpreter in Racket

Racket introduction home assignment

## Required abstract syntax

- $V := \{x, y, z, ...\}$
- $C := \mathbb{N}$
- $\Lambda := C | V | \Lambda \, \Lambda | \lambda V. \Lambda | if \, \Lambda \, then \, \Lambda \, else \, \Lambda | Op \, \Lambda_1 \, \Lambda_2 \, ... \, \Lambda_n | Y \, \Lambda$

## Actual syntax

- single `exit` exits the interpreter, otherwise an `<exp>` is parsed
- keywords: `if`, `then`, `else`, `exit`
- `<exp> := <lambda> | <if-then-else> | <call-func-by-id> | <app-chain>`
- `<lambda> := "\" <id> "|" <exp>`
- `<if-then-else> := "if" <atomic-exp> "then" <atomic-exp> "else" <atomic-exp>`
- `<app-chain> := <atomic-exp>+`
- `<atomic-exp> := "(" <exp> ")" | <num> | <id>`
- `<id>` is a Racket identifier (except keywords)
- `<num>` $\in \mathbb{Z}$

## Running

`racket lc-interpreter.rkt` starts a REPL which executes single-line commands

## Notes

- external functions defined in Racket may be referenced and called through application (however, they will be forcibly curried exactly for this purpose)
- if several lambdas bind a variable by the same name then the innermost one would be taken