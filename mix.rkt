#lang racket

(require "auxiliary_functions.rkt")
(require "int.rkt")
(provide mix do-mix)

(define mix '((read program division vars_0)
              (init (:= program-point_0 (car (initial-bb program)))
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
                              (:= print (printf "finding ~v\n" program-point))
                              (goto lookup-pp-main))
              (lookup-pp-main (:= program-point-cur (car program-points))
                              (:= program-points (cdr program-points))
                              (goto lookup-pp-main-cond))
              (lookup-pp-main-cond 
                              (if (equal? program-point program-point-cur) main-loop-body-cont lookup-pp-main))
              (main-loop-body-cont
                              (:= bb (bb-lookup program program-point-cur))
                              (:= labels (add-label labels cur))
                              (:= label (get-label labels cur))
                              (:= code `(,label))
                              (goto inner-loop-check))
                
              (inner-loop-check (if (empty? bb) inner-loop-exit inner-loop-body))
        
              (inner-loop-body (:= Inst (car bb))
                               (:= bb (cdr bb))
        
                               (if (equal? ':= (car Inst)) inner-loop-assign inner-loop-match-goto))
            
              (inner-loop-assign (:= x (cadr Inst))
                                 (:= expr (caddr Inst))
                                 (if (set-member? division x) inner-loop-assign-static inner-loop-assign-dynamic))
        
              (inner-loop-assign-static (:= vars (st-set vars x (eval-exp vars expr)))
                                        (goto inner-loop-check))
              
              (inner-loop-assign-dynamic (:= code (cons `(:= ,x ,(reduce expr vars)) code))
                                         (goto inner-loop-check))
        
              (inner-loop-match-goto (if (equal? 'goto (car Inst)) inner-loop-goto inner-loop-match-if))
        
              (inner-loop-goto (:= bb (bb-lookup program (cadr Inst)))
                               (goto inner-loop-check))
        
              (inner-loop-match-if (if (equal? 'if (car Inst)) inner-loop-if inner-loop-match-return))
        
              (inner-loop-if (:= expr (cadr Inst))
                             (:= then-label (caddr Inst))
                             (:= else-label (cadddr Inst))
                             (if (static-by-div? expr vars) inner-loop-if-static inner-loop-if-dynamic))
        
              (inner-loop-if-static (if (eval-exp vars expr) inner-loop-if-static-then inner-loop-if-static-else))
        
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
                                     (:= code (cons `(if ,(reduce expr vars) ,then-dynamic-label ,else-dynamic-label) code))
                                     (goto inner-loop-check))
        
              (inner-loop-match-return (if (equal? 'return (car Inst)) inner-loop-return inner-loop-no-match))
        
              (inner-loop-no-match (return `(instruction not matched: ,Inst)))
        
              (inner-loop-return (:= expr (cadr Inst))
                                 (:= code (cons `(return ,(reduce expr vars)) code))
                                 (goto inner-loop-check))
        
              (inner-loop-exit 
                              ;;;  (:= print (pretty-print (reverse code)))
                               (:= residual-code (cons (reverse code) residual-code))
                               (goto main-loop-check))
                               
              (main-loop-exit (return (reverse residual-code)))))

; convenient mix applier
(define (do-mix prog div sv)
      (let* ([div-in (list->set div)]
            [bound-pred (lambda (var) (st-bound? sv var))]
            [read-out (cons 'read (filter-not bound-pred (cdar prog)))])
           (cons read-out (int mix `(,prog ,div-in ,sv)))))