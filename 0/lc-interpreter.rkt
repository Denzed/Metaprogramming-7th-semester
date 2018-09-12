#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(struct Num (n) #:transparent)
(struct Var (s) #:transparent)
(struct App (l r) #:transparent)
(struct Lmb (var expr) #:transparent)
(struct IfThenElse (cond thenExpr elseExpr) #:transparent)
(struct OpCall (func args) #:transparent)
(struct Fix (f) #:transparent)

(define-tokens value-tokens (NUM VAR OPER))
(define-empty-tokens op-tokens (OP CP EOF YCOMB LMB LMB_DOT IF THEN ELSE EXIT))

(define fix (lambda (f) (f (lambda (x) (fix f) x))))

(define-lex-abbrevs
 (digit (:/ "0" "9"))
 (operator (:or "+" "-" "*" "/" "<" ">")))
 
(define lc-lexer
  (lexer
    [(eof) 'EOF]
    ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
    ;; result of that operation.  This effectively skips all whitespace.
    [whitespace (lc-lexer input-port)]
    ["(" 'OP]
    [")" 'CP]
    ["Y" 'YCOMB]
    ["λ" 'LMB]
    ["." 'LMB_DOT]
    ["if" 'IF]
    ["then" 'THEN]
    ["else" 'ELSE]
    ["exit" 'EXIT]
    [lower-case (token-VAR (string->symbol lexeme))]
    [(:+ digit) (token-NUM (string->number lexeme))]
    [operator (token-OPER (string->symbol lexeme))]))

(define lc-parser
  (parser
    
    (start start)
    (end EOF)
    (tokens value-tokens op-tokens)
    (error (lambda (a b c) (eprintf "~a ~a ~a" a b c)))
    
    (grammar
  
      (start 
        [() #f]
        [(EXIT) (exit)]
        [(exp) $1])
    
      (args 
        [() null] 
        [(atomic-exp args) (cons $1 $2)])

      (atomic-exp
        [(NUM) (Num $1)]
        [(VAR) (Var $1)]
        [(OP exp CP) $2])
      
      (app-chain
        [(app-chain atomic-exp) (App $1 $2)]
        [(atomic-exp) $1])

      (exp 
        [(app-chain) $1]
        [(LMB VAR LMB_DOT exp) (Lmb $2 $4)]
        [(IF exp THEN exp ELSE exp) (IfThenElse $2 $4 $6)]
        [(YCOMB exp) (Fix $2)]
        [(OPER args) (OpCall $1 $2)]))))

(define (vars-empty x) 
  (error (format "no such variable ~a" x)))

(define (vars-add vars v n) 
  (lambda (w) (if (equal? v w) n (vars w))))

(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->procedure s)
  (eval s ns))

(define (lc-eval vars expr) 
  (cond [(Num? expr) (Num-n expr)]
        [(Var? expr) (vars (Var-s expr))]
        [(App? expr) ((lc-eval vars (App-l expr)) (lc-eval vars (App-r expr)))]
        [(Lmb? expr) (lambda (n) (lc-eval (vars-add vars (Lmb-var expr) n) (Lmb-expr expr)))]
        [(IfThenElse? expr) (cond 
          [(equal? 0 (lc-eval vars (IfThenElse-cond expr))) (lc-eval vars IfThenElse-elseExpr)]
          [else                                          (lc-eval vars IfThenElse-thenExpr)])]
        [(OpCall? expr) (let 
          ([fname (OpCall-func expr)] 
           [fargs (map (lambda (arg) (lc-eval vars arg)) (OpCall-args expr))])
          (apply (string->procedure fname) fargs))]
        [(Fix? expr) (fix (lc-eval vars (Fix-f expr)))]))


(let ([one-line
  (lambda (ip)
    (let ((parsed (lc-parser (lambda () (lc-lexer ip)))))
      (let ((result (lc-eval vars-empty parsed)))
        result)))])

  (letrec ([loop (lambda () 
    (define l (read-line (current-input-port) 'any))
    (printf "~v\n" (one-line (open-input-string l)))
    (loop))])
    
    (loop)))
