#lang racket

(require "auxiliary_functions.rkt")
(require "int.rkt")
(provide Mix mix do-mix)

(define mix '((read program division vars_0)
              (init 
                  ;;;   (:= print (printf "PROGRAM: ~v\n" program))
                  ;;;   (:= print (printf "VARS: "))
                  ;;;   (:= print (pretty-print vars_0))
                  ;;;   (:= print (printf "TEST: ~v\n" (car program)))
                    (:= program-point_0 (car (initial-bb program)))
                    (:= program (initial-prog program))
                    (:= labels (hash))
                    (:= pending (set (cons program-point_0 vars_0)))
                    (:= marked (set))
                    (:= residual-code '())
                    (goto main-loop-check))
                
              (main-loop-check (if (set-empty? pending) main-loop-exit main-loop-body))
        
              (main-loop-body (:= cur (set-first pending))
                              ;;; (:= print (printf "CUR: ~v\n" cur))
                              (:= pending (set-rest pending))
                              (:= program-point (car cur))
                              (:= vars (cdr cur))
                              (:= program-points (prog-points program))
                              (goto lookup-pp-main))
              (lookup-pp-main (:= program-point-cur (car program-points))
                              (:= program-points (cdr program-points))
                              (goto lookup-pp-main-cond))
              (lookup-pp-main-cond 
                              (if (equal? program-point program-point-cur) main-loop-body-cont lookup-pp-main-cond2))
              (lookup-pp-main-cond2
                              (if (empty? program-points) lookup-pp-main-fail lookup-pp-main))
              (lookup-pp-main-fail 
                              (return (list 'program 'point 'not 'found: program-point)))
              (main-loop-body-cont
                              (:= bb (bb-lookup program program-point-cur))
                              (:= labels (add-label labels cur))
                              (:= label (get-label labels cur))
                              (:= code (list label))
                              (goto inner-loop-check))
                
              (inner-loop-check (if (empty? bb) inner-loop-exit inner-loop-body))
        
              (inner-loop-body (:= Inst (car bb))
                               (:= bb (cdr bb))
        
                               (if (equal? ':= (car Inst)) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign (:= x (cadr Inst))
                                 (:= expr (caddr Inst))
                              ;;;    (if (set-member? (division-at-pp division program-point-cur) x)
                              ;;;        inner-loop-assign-static 
                              ;;;        inner-loop-assign-dynamic)
                                 (:= static-val (eval-static-or-#f expr vars))
                                 (if static-val inner-loop-assign-static inner-loop-assign-dynamic)
                                 )
        
              (inner-loop-assign-static 
                                    ;;;     (:= static-val (eval-exp vars expr))
                                        (:= static-val (car static-val))
                                        (:= vars (st-set vars x static-val))
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic 
                                    ;;;      (:= print (printf "[~v := ~v NOT STATIC]" x expr))
                                         (:= code (cons (list ':= x (reduce expr vars)) code))
                                         (:= vars (st-remove vars x))
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (if (equal? 'goto (car Inst)) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= program-point-goto (cadr Inst))
                               (:= bb (bb-lookup program program-point-goto))
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (equal? 'if (car Inst)) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (:= expr (cadr Inst))
                             (:= then-label (caddr Inst))
                             (:= else-label (cadddr Inst))
                             (:= static-val (eval-static-or-#f expr vars))
                             (if static-val inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (if (car static-val) inner-loop-if-static-then inner-loop-if-static-else))
        
              (inner-loop-if-static-then (:= bb (bb-lookup program then-label))
                                                (goto inner-loop-check))

              (inner-loop-if-static-else (:= bb (bb-lookup program else-label))
                                         (goto inner-loop-check))
        
              (inner-loop-if-dynamic (:= then-out-label (cons then-label vars))
                                     (:= else-out-label (cons else-label vars))  
                                     (:= labels (add-label (add-label labels then-out-label) else-out-label))
                                     (:= then-dynamic-label (get-label labels then-out-label))
                                     (:= else-dynamic-label (get-label labels else-out-label))
                                     (:= to-add (set then-out-label else-out-label))
                                     (:= to-add (set-subtract to-add marked))
                                     (:= marked (set-union marked to-add))
                                     (:= pending (set-union pending to-add))
                                     (:= code (cons (list 'if (reduce expr vars) then-dynamic-label else-dynamic-label) code))
                                     (goto inner-loop-check))
        
              (inner-loop-match-return (if (equal? 'return (car Inst)) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (return (list 'instruction 'not 'matched: Inst)))
        
              (inner-loop-return (:= expr (cadr Inst))
                                 (:= code (cons (list 'return (reduce expr vars)) code))
                                 (goto inner-loop-check))
        
              (inner-loop-exit (:= residual-code (cons (reverse code) residual-code))
                               (goto main-loop-check))
                               
              (main-loop-exit (return (reverse residual-code)))))

(define Mix '((read program division vars_0)
              (init 
                  ;;;   (:= print (printf "PROGRAM: ~v\n" program))
                  ;;;   (:= print (printf "VARS: "))
                  ;;;   (:= print (pretty-print vars_0))
                  ;;;   (:= print (printf "TEST: ~v\n" (car program)))
                    (:= program-point_0 (car (initial-bb program)))
                    (:= program (initial-prog program))
                    (:= labels (hash))
                    (:= pending (set (cons program-point_0 vars_0)))
                    (:= marked (set))
                    (:= residual-code '())
                    (goto main-loop-check))
                
              (main-loop-check (if (set-empty? pending) main-loop-exit main-loop-body))
        
              (main-loop-body (:= cur (set-first pending))
                              (:= pending (set-rest pending))
                              (:= program-point (car cur))
                              (:= vars (cdr cur))
                              (:= marked (set-add marked cur))
                              (:= program-points (prog-points program))
                              (goto lookup-pp-main))
              (lookup-pp-main (:= program-point-cur (car program-points))
                              (:= program-points (cdr program-points))
                              (goto lookup-pp-main-cond))
              (lookup-pp-main-cond 
                              (if (equal? program-point program-point-cur) main-loop-body-cont lookup-pp-main-cond2))
              (lookup-pp-main-cond2
                              (if (empty? program-points) lookup-pp-main-fail lookup-pp-main))
              (lookup-pp-main-fail 
                              (return (list 'program 'point 'not 'found: program-point)))
              (main-loop-body-cont
                              (:= bb (bb-lookup program program-point-cur))
                              (:= labels (add-label labels cur))
                              (:= label (get-label labels cur))
                              (:= code (list label))
                              (goto inner-loop-check))
                
              (inner-loop-check (if (empty? bb) inner-loop-exit inner-loop-body))
        
              (inner-loop-body (:= Inst (car bb))
                               (:= bb (cdr bb))
        
                               (if (equal? ':= (car Inst)) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign (:= x (cadr Inst))
                                 (:= expr (caddr Inst))
                                 (:= static-val (eval-static-or-#f expr vars))
                              ;;;    (set-member? division x)
                                 (if static-val inner-loop-assign-static inner-loop-assign-dynamic))
        
              (inner-loop-assign-static (:= vars (st-set vars x (car static-val)))
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic 
                                    ;;;      (:= print (printf "[~v := ~v NOT STATIC]" x expr))
                                         (:= code (cons (list ':= x (reduce expr vars)) code))
                                         (:= vars (st-remove vars x))
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (if (equal? 'goto (car Inst)) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= bb (bb-lookup program (cadr Inst)))
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (equal? 'if (car Inst)) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (:= expr (cadr Inst))
                             (:= then-label (caddr Inst))
                             (:= else-label (cadddr Inst))
                             (:= static-val (eval-static-or-#f expr vars))
                             (if static-val inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (if (car static-val) inner-loop-if-static-then inner-loop-if-static-else))
        
              (inner-loop-if-static-then (:= bb (bb-lookup program then-label))
                                                (goto inner-loop-check))

              (inner-loop-if-static-else (:= bb (bb-lookup program else-label))
                                         (goto inner-loop-check))
        
              (inner-loop-if-dynamic (:= then-out-label (cons then-label vars))
                                     (:= else-out-label (cons else-label vars))  
                                     (:= labels (add-label (add-label labels then-out-label) else-out-label))
                                     (:= then-dynamic-label (get-label labels then-out-label))
                                     (:= else-dynamic-label (get-label labels else-out-label))
                                     (:= to-add (set then-out-label else-out-label))
                                     (:= to-add (set-subtract to-add marked))
                                     (:= pending (set-union pending to-add))
                                     (:= code (cons (list 'if (reduce expr vars) then-dynamic-label else-dynamic-label) code))
                                     (goto inner-loop-check))
        
              (inner-loop-match-return (if (equal? 'return (car Inst)) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (return (list 'instruction 'not 'matched: Inst)))
        
              (inner-loop-return (:= expr (cadr Inst))
                                 (:= code (cons (list 'return (reduce expr vars)) code))
                                 (goto inner-loop-check))
        
              (inner-loop-exit (:= residual-code (cons (reverse code) residual-code))
                               (goto main-loop-check))
                               
              (main-loop-exit (return (reverse residual-code)))))

; convenient mix applier
(define (do-mix prog div sv) 
        (let ([remaining-args (filter-not 
                                          (lambda (var) (st-bound? sv var))
                                          (cdar prog))]) 
             (cons 
                   (cons 'read remaining-args) 
                   (int 
                        mix 
                        (list 
                              prog
                              div
                              sv)))))