#lang racket

(provide (all-defined-out))

; for environment
(define st-lookup dict-ref)
(define st-bound? dict-has-key?)
(define st-set
  (lambda (st x e)
      (dict-set st x (cons 'quote (list e)))))
(define st-empty  #hash())
(define initial-st
  (lambda (vars d)
    (if (equal? (length vars) (length d))
        (for/fold ([st st-empty])
                  ([i vars]
                   [j d])
          (st-set st i j))
        (error "initial-st error: program arity mismatch"))))
(define (initial-bb p) (cadr p))

; for basic_blocks
(define bb-lookup dict-ref)
(define bb-set    dict-set)
(define bb-empty  #hash())
(define initial-prog
  (lambda (p)
    (for/fold ([bbs bb-empty])
              ([i (cdr p)])
      (bb-set bbs (car i) (cdr i)))))

; my-eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define my-eval
  (lambda (e)
    (eval e ns)))

; eval expression in current environment
(define (subst st e)
  (match e [`(,x . ,y) `(,(subst st x) . ,(subst st y))]
           [`,x        (if (st-bound? st x) (st-lookup st x) x)]))
(define (eval-exp st e)
  (let ([ee (subst st e)]) (my-eval ee)))

; for goto in TM
(define Car 
  (lambda (Band) (if (null? Band) 'B (car Band))))

(define Cdr 
  (lambda (Band) (if (null? Band) '() (cdr Band))))

(define new-Qtail 
  (lambda (Q label) 
    (member label Q
      (lambda (s t) (equal? s (car t))))))

; mix functions
; add new label to dictionary
(define (add-label labels key) 
    (if (dict-has-key? labels key) labels (dict-set labels key `(label ,(dict-count labels)))))

; get mapped label from dictionary
(define get-label dict-ref)

; check if expression is static by division
(define (static-by-div? expr sv)
    (with-handlers ([exn:fail:contract:variable? (lambda (err) #f)]) 
        (eval-exp sv expr)
        #t))

; try to reduce expression using static information
(define reduce
  (lambda (e div) (subst div e))) ; currently no partial computation implemented