#lang racket

(require "auxiliary_functions.rkt")
(provide mix)

(define mix '((read program division vars_0)
              (init (:= program_point_0 (initial-bb program))
                    (:= pending (set (cons program_point_0 vars_0)))
                    (:= marked (set))
                    (goto main-loop-check))
                
              (main-loop-check (if (set-empty? pending) main-loop-exit main-loop-body))
        
              (main-loop-body (:= cur (set-first pending))
                              (:= pending (set-rest pending))
                              (:= program_point (car cur))
                              (:= vars (cdr cur))
                              (:= marked (set-add marked cur))
                              (:= bb (bb-lookup program_point program))
                              (:= code (initial-code-for-bb program_point vars))
                              (goto inner-loop-check))
                
              (inner-loop-check (if (empty? bb) inner-loop-exit inner-loop-body))
        
              (inner-loop-body (:= Inst (car bb))
                               (:= bb (cdr bb))
        
                               (if (matches-assign? Inst) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign (:= x (assign-get-var Inst))
                                 (:= expr (assign-get-expr Inst))
                                 (if (static-by-div? x division) inner-loop-assign-static inner-loop-assign-dynamic))
        
              (inner-loop-assign-static (:= vars (st-set vars x (eval-exp vars expr)))
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic (:= code (code-extend code '(:= x (reduce expr vars))))
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (if (matches-goto? Inst) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= bb (bb-lookup program (goto-get-pp Inst)))
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (matches-if? Inst) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (:= expr (if-get-cond Inst))
                             (if (static-by-div? expr) inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (if (false? (eval-exp vars expr)) inner-loop-if-static-else inner-loop-if-static-then))
        
              (inner-loop-if-static-then (:= bb (bb-lookup program (if-get-then-pp Inst)))
                                         (goto inner-loop-check))
        
              (inner-loop-if-static-else (:= bb (bb-lookup program (if-get-else-pp Inst)))
                                         (goto inner-loop-check))
        
              (inner-loop-if-dynamic (:= to-add (set (cons (if-get-then-pp Inst) vars) (cons (if-get-else-pp Inst) vars))
                                     (:= to-add (set-subtract to-add marked))
                                     (:= pending (set-union pending to-add))
                                     (:= code (code-extend code '(if (reduce expr vars) (if-get-then-pp Inst) (if-get-else-pp Inst))))
                                     (goto inner-loop-check)))
        
              (inner-loop-match-return (if (matches-return? Inst) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (error "expression not matched"))
        
              (inner-loop-return (:= code (code-extend code '(return (reduce expr vars))))
                                 (goto inner-loop-check))
        
              (inner-loop-exit (:= residual_code (extend residual_code code))
                               (goto main-loop-check))
                               
              (main-loop-exit (return resudual_code))))
