#lang racket


(require "auxiliary_functions.rkt")
(require "int.rkt")
(provide mix do-mix)

(define mix '((read program division vars_0)
              (init 
                  ;;;   (:= print (printf "PROGRAM: ~v\n" program))
                  ;;;   (:= print (printf "VARS: "))
                  ;;;   (:= print (pretty-print vars_0))
                  ;;;   (:= print (printf "TEST: ~v\n" (car program)))
                    (:= program-point_0 (car (initial-bb program)))
                    (:= initial-label (cons program-point_0 vars_0))
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
                
              (main-loop-check (if (stream-empty? pending) main-loop-exit main-loop-body))
        
              (main-loop-body (:= cur (stream-first pending))

                              ;;; (:= print (printf "PENDING SIZE IS ~v\n" (stream-length pending)))

                              (:= pending (stream-rest pending))
                              (if (set-member? marked cur) main-loop-check main-loop-body-cont1))
              (main-loop-body-cont1 
                              (:= marked (set-add marked cur))
                              (:= program-point (car cur))
                              (:= vars (cdr cur))
                              (:= program-point-cur program-point_0)
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
                              (:= bb (bb-lookup program program-point-cur))
                              (:= code (list (get-label labels cur)))
                              
                              ;;; (:= print (printf "CUR: ~v\n" cur))
                              ;;; (:= print (pretty-print labels))
                              ;;; (:= print (printf "~v:\n" program-point-cur))
                               
                              (goto inner-loop-check))
                
              (inner-loop-check (if (empty? bb) inner-loop-exit inner-loop-body))
        
              (inner-loop-body (:= Inst (car bb))
                               (:= bb (cdr bb))

                              ;;;  (:= print (pretty-print vars))
                              ;;;  (:= print (pretty-print Inst))
        
                               (if (equal? ':= (car Inst)) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign 
                              ;;;    (:= print (printf "ASSIGN: ~v := ~v => ~v\n" (cadr Inst) (caddr Inst) static-val))
                                 (if (set-member? (division-at-pp division program-point-cur) (cadr Inst))
                                     inner-loop-assign-static 
                                     inner-loop-assign-dynamic))
        
              (inner-loop-assign-static 
                                        (:= static-val (eval-exp vars (caddr Inst)))
                                    ;;;     (:= static-val (car static-val))
                                        (:= vars (st-set vars (cadr Inst) static-val))
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic 
                                    ;;;      (:= print (printf "[~v := ~v NOT STATIC]" (cadr Inst) (caddr Inst)))
                                         (:= code (cons (list ':= (cadr Inst) (reduce (caddr Inst) vars)) code))
                                         (:= vars (st-remove vars (cadr Inst)))
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (if (equal? 'goto (car Inst)) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= bb (bb-lookup program (cadr Inst)))
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (equal? 'if (car Inst)) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (:= expr (cadr Inst))
                             (:= then-out-label (cons (caddr Inst) vars))
                             (:= else-out-label (cons (cadddr Inst) vars))
                             (:= static-val (eval-static-or-#f expr vars))
                        ;;;      (:= print (printf "IF ~v then ~v else ~v\n" expr then-label else-label))
                        ;;;      (:= print (printf "\t@ VARS"))
                        ;;;      (:= print (pretty-print vars))
                        ;;;      (:= print (printf "STATIC: ~v\n" static-val))
                             (if static-val inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (:= static-val (car static-val))
                                    (if static-val inner-loop-if-static-then inner-loop-if-static-else))
        
              (inner-loop-if-static-then (:= bb (bb-lookup program (caddr Inst)))
                                         (goto inner-loop-check))

              (inner-loop-if-static-else (:= bb (bb-lookup program (cadddr Inst)))
                                         (goto inner-loop-check))
        
              (inner-loop-if-dynamic (:= labels (add-labels labels then-out-label else-out-label))
                                    ;;;  (:= print (printf "ADDING: ~v ~v\n" then-out-label else-out-label))
                                    ;;;  (:= print (pretty-print labels))
                                     (:= then-dynamic-label (get-label labels then-out-label))
                                     (:= else-dynamic-label (get-label labels else-out-label))
                                     (:= pending (stream-append pending (list then-out-label else-out-label)))
                                     (:= code (cons (list 'if (reduce expr vars) then-dynamic-label else-dynamic-label) code))
                                     (goto inner-loop-check))
        
              (inner-loop-match-return (if (equal? 'return (car Inst)) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (return `(instruction not matched: ,Inst)))
        
              (inner-loop-return (:= code (cons (list 'return (reduce (cadr Inst) vars)) code))
                                 (goto inner-loop-check))
        
              (inner-loop-exit (:= residual-code (cons (reverse code) residual-code))
                              ;;;  (:= print (pretty-print (reverse code)))
                               (goto main-loop-check))
                               
              (main-loop-exit (return (reverse residual-code)))))

; convenient mix applier
(define (do-mix prog div sv) (int mix (list prog div sv)))