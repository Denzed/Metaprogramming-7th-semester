#lang racket

(require "auxiliary_functions.rkt")
(require "int.rkt")
(provide mix do-mix)

(define mix '((read program division vars_0)
              (init (:= initial-label (cons (car (initial-bb program)) vars_0))
                    (:= labels (add-labels empty-labels initial-label))
                    (:= pending (stream initial-label))
                    (:= marked (set))
                    (:= residual-code 
                        (cons 
                              (cons 
                                    'read
                                    (filter-not (lambda (var) (st-bound? vars_0 var)) (cdar program))) 
                              '()))
                    (:= program (initial-prog program))
                    (goto main-loop-check))
                
              (main-loop-check (:= pending (clear-initial-marked marked pending))
                               (:= program-point-cur (car (prog-points program)))
                               (:= bb-index -1)
                               (:= Inst '())
                               (if (stream-empty? pending) main-loop-exit main-loop-body))
        
              (main-loop-body (:= cur (stream-first pending))
                              (:= pending (stream-rest pending))
                              (:= marked (set-add marked cur))
                              (:= program-point (car cur))
                              (:= vars (cdr cur))
                              (goto lookup-pp-main-cond))
              (lookup-pp-main (:= program-point-cur (prog-point-next program program-point-cur))
                              (goto lookup-pp-main-cond))
              (lookup-pp-main-cond 
                              (if program-point-cur lookup-pp-main-cond2 lookup-pp-main-fail))
              (lookup-pp-main-cond2
                              (if (equal? program-point program-point-cur) main-loop-body-cont2 lookup-pp-main))
              (lookup-pp-main-fail 
                              (return `(pp not found: ,program-point)))
              (main-loop-body-cont2
                              (:= code (list (get-label labels cur)))
                              (goto inner-loop-check))
                
              (inner-loop-check (:= bb-index (+ bb-index 1))
                                (if (equal? bb-index (length (bb-lookup program program-point-cur))) 
                                    inner-loop-exit 
                                    inner-loop-body))
        
              (inner-loop-body (:= Inst (list-ref (bb-lookup program program-point-cur) bb-index))
                               (if (equal? ':= (car Inst)) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign (if (set-member? 
                                                  (division-at-pp division program-point-cur) 
                                                  (cadr Inst)) 
                                     inner-loop-assign-static 
                                     inner-loop-assign-dynamic))
        
              (inner-loop-assign-static (:= vars (st-set vars (cadr Inst) (eval-exp vars (caddr Inst))))
                                        (:= Inst '())
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic (:= code (cons (list ':= (cadr Inst) (reduce (caddr Inst) vars)) code))
                                         (:= Inst '())
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (:= program-point-cur (car (prog-points program)))
                                     (:= bb-index -1)
                                     (if (equal? 'goto (car Inst)) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= program-point-cur (cadr Inst))
                               (:= Inst '())
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (equal? 'if (car Inst)) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (if (static-expr? (cadr Inst) vars) inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (if (eval-exp vars (cadr Inst)) inner-loop-if-static-then inner-loop-if-static-else))
        
              (inner-loop-if-static-then (:= program-point-cur (caddr Inst))
                                         (:= Inst '())
                                         (goto inner-loop-check))

              (inner-loop-if-static-else (:= program-point-cur (cadddr Inst))
                                         (:= Inst '())
                                         (goto inner-loop-check))
        
              (inner-loop-if-dynamic (:= then-out-label (cons (caddr Inst) vars))
                                     (:= else-out-label (cons (cadddr Inst) vars))
                                     (:= labels (add-labels labels then-out-label else-out-label))
                                     (:= then-dynamic-label (get-label labels then-out-label))
                                     (:= else-dynamic-label (get-label labels else-out-label))
                                     (:= pending (stream-append pending (list then-out-label else-out-label)))
                                     (:= code (cons (list 'if (reduce (cadr Inst) vars) then-dynamic-label else-dynamic-label) code))
                                     (:= Inst '())
                                     (goto inner-loop-exit))
        
              (inner-loop-match-return (if (equal? 'return (car Inst)) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (return `(instruction not matched: ,Inst)))
        
              (inner-loop-return (:= code (cons (list 'return (reduce (cadr Inst) vars)) code))
                                 (:= Inst '())
                                 (goto inner-loop-exit))
        
              (inner-loop-exit (:= residual-code (cons (reverse code) residual-code))
                               (goto main-loop-check))
                               
              (main-loop-exit (return (reverse residual-code)))))

; convenient mix applier
(define (do-mix prog div sv) (int mix (list prog div sv)))