#lang racket

(provide (all-defined-out))

; for environment
(define st-lookup dict-ref)
(define st-bound? dict-has-key?)
(define (st-set st x e) (dict-set st x e))
(define (st-set* st . xe) (if (null? xe) 
                              st 
                              (let ([x (car xe)]
                                    [e (cadr xe)]
                                    [xe-out (cddr xe)])
                                   (apply st-set* (st-set st x e) xe-out))))
(define st-remove dict-remove)
(define st-empty  #hash())
(define initial-st
  (lambda (vars d)
    (if (equal? (length vars) (length d))
        (for/fold ([st st-empty])
                  ([i vars]
                   [j d])
          (st-set st i j))
        (error "initial-st error: program arity mismatch"))))

; for basic_blocks
(define bb-lookup dict-ref)
(define bb-set    dict-set)
(define bb-empty  #hash())
(define (initial-bb p) (cadr p))
(define (initial-prog p)
    (for/fold ([bbs bb-empty])
              ([i (cdr p)])
      (bb-set bbs (car i) (cdr i))))

(define (prog-points prog) 
        (sort (sequence->list (in-dict-keys prog)) 
              string<? 
              #:key (lambda (d) (format "~s" d))))
(define (prog-point-next prog point)
        (define tail (member point (prog-points prog)))
        (cond [(not tail)          #f         ]
              [(< (length tail) 2) #f         ]
              [else                (cadr tail)]))

; for division
(define (division-at-pp div pp) (if (dict? div) (dict-ref div pp) div))

; my-eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define my-eval
  (lambda (e)
    (eval e ns)))

; eval expression in current environment
(define (subst st e)
  (match e [`(,x . ,y) `(,(subst st x) . ,(subst st y))]
           [`,x        (if (st-bound? st x) `',(st-lookup st x) x)]))
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
(define empty-labels (hash))

(define (add-labels labels . keys)
  (define (add-label labels key) 
    (if (dict-has-key? labels key) labels (dict-set labels key `(label ,(dict-count labels)))))

  (if (null? keys) 
      labels 
      (apply add-labels 
              (add-label labels (car keys)) 
              (cdr keys))))

; get mapped label from dictionary
(define get-label dict-ref)

; check if expression is static by division
(define (eval-static-or-#f expr sv)
    (with-handlers ([exn:fail:contract:variable? (lambda (err) #f)]) 
        (list (eval-exp sv expr))))

; try to reduce expression using static information
(define (reduce e st)
  ;;; (printf "REDUCE: ~v\n" e)
  (match e [`(,x . ,y) `(,(reduce x st) . ,(reduce y st))]
           [`,x        (if (st-bound? st x) `',(st-lookup st x) x)]))

; reduce v2
;;; (define (reduce expr sv)
;;;   (define (reduce-inner e st)
;;;           (match e [`(,x . ,y) `(,(reduce-inner x st) . ,(reduce-inner y st))]
;;;                    [`,x        (if (st-bound? st x) `',(st-lookup st x) x)]))
  
;;;   (let ([maybe-result (eval-static-or-#f expr sv)])
;;;        (if maybe-result `',(car maybe-result) (reduce-inner expr sv))))