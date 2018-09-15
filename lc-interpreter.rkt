#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(struct Num (n) #:transparent)
(struct Var (v) #:transparent)
(struct App (l r) #:transparent)
(struct Lmb (var expr) #:transparent)
(struct IfThenElse (cond thenExpr elseExpr) #:transparent)

(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens (OP CP EOF LMB LMB_BODY IF THEN ELSE EXIT))

(define fix (lambda (f) (f (lambda (x) (fix f) x))))

(define-lex-abbrevs
 (digit (:/ "0" "9"))
 (number (:seq (:? "-") (:+ digit)))
 (identifier 
   (:- 
     (:+ (:~ (:or "(" ")" "[" "]" "{" "}" "\"" "," "`" ";" "#" "|" "\\" whitespace))) 
     number
     "if"
     "then"
     "else"
     "exit")))
 
(define lc-lexer
  (lexer
    [(eof) 'EOF]
    ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
    ;; result of that operation.  This effectively skips all whitespace.
    [whitespace (lc-lexer input-port)]
    ["(" 'OP]
    [")" 'CP]
    ["\\" 'LMB]
    ["|" 'LMB_BODY]
    ["if" 'IF]
    ["then" 'THEN]
    ["else" 'ELSE]
    ["exit" 'EXIT]
    [number (token-NUM (string->number lexeme))]
    [identifier (token-ID (string->symbol lexeme))]
    [any-char (begin 
      (eprintf "Invalid symbol encountered\n")
      'EOF)]))

(define lc-parser
  (parser
    
    (start start)
    (end EOF)
    (tokens value-tokens op-tokens)
    (error (lambda (a b c) (eprintf "Invalid syntax\n")))
    
    (grammar
  
      (start 
        [() (void)]
        [(EXIT) (exit)]
        [(exp) $1])

      (atomic-exp
        [(NUM) (Num $1)]
        [(ID) (Var $1)]
        [(OP exp CP) $2])

      (app-chain
        [(app-chain atomic-exp) (App $1 $2)]
        [(atomic-exp) $1])
      
      (exp 
        [(LMB ID LMB_BODY exp) (Lmb $2 $4)]
        [(IF atomic-exp THEN atomic-exp ELSE atomic-exp) (IfThenElse $2 $4 $6)]
        [(app-chain) $1]))))

(define ns (variable-reference->namespace (#%variable-reference)))

(define (get-free-var s)
  (define value (eval s ns))
  (cond 
    [(procedure? value) (curry value)]
    [else value]))

(define (vars-empty x) 
  (get-free-var x))

(define (vars-add vars v n) 
  (lambda (w) (if (equal? v w) n (vars w))))

(define (lc-eval vars expr) 
  (cond [(Num? expr) (Num-n expr)]
        [(Var? expr) (vars (Var-v expr))]
        [(App? expr) ((lc-eval vars (App-l expr)) (lc-eval vars (App-r expr)))]
        [(Lmb? expr) (lambda (n) (lc-eval (vars-add vars (Lmb-var expr) n) (Lmb-expr expr)))]
        [(IfThenElse? expr) (cond 
          [(equal? 0 (lc-eval vars (IfThenElse-cond expr))) (lc-eval vars IfThenElse-elseExpr)]
          [else                                          (lc-eval vars IfThenElse-thenExpr)])]))


(let ([one-line
  (lambda (ip)
    (let ((parsed (lc-parser (lambda () (lc-lexer ip)))))
      (let ((result (lc-eval vars-empty parsed)))
        result)))])

  (letrec ([loop (lambda () 
    (define l (read-line (current-input-port) 'any))
    (define result (one-line (open-input-string l)))
    (cond 
      [(not (void? result)) (printf "~v\n" result)]
      [else (printf "\n")])

    (loop))])
    
    (loop)))
