#lang racket

(require "auxiliary_functions.rkt")
(provide int int-TM)

(define fill_in
  (lambda () '()))

#;(FlowChart is a simple imperative language
   <Program>    ::=   read <Var> ... <Var>: <BasicBlock>+
   <BasicBlock> ::=   <Label>: <Assignment>* <Jump>
   <Label>      ::=   any identifier or value
   <Assignment> ::=   := <Var> <Expression>
   <Jump>       ::=   goto <Label> | if <Expr> <Label> <Label> | return <Expression>
   <Expression> ::=   <Const> | <Var> | <Op> <Expr> ... <Expr>
   <Const>      ::=   any quoted data
   <Op>         ::=   any racket function
   Concrete syntax --- see example from examples.rkt

   int      --- FlowChart interpreter
   int-bb   --- basic block interpreter
   int-jump --- jump interpreter
   int-assn --- assignment interpreter
   int-TM   --- Turing Machine Interpreter on FlowChart
   int :: program -> data -> result
   (see example from examples.rkt)
   
   st is a program state (see dict from racket docs)
   all subsidiary functions are defined in auxiliary_functions.rkt

   function eval-exp (from auxiliary_functions) :: st -> expr -> result
     evaluates expression expr in environment st
   )

(define int
  (lambda (p d)
    (let ([initial-vars (match (car p) [(list 'read vars ...) vars])])
      (int-bb (initial-prog p) (initial-st initial-vars d) (cdr (initial-bb p))))))

(define int-bb
  (lambda (prog st bb)
    (match bb [(list assns ... jump)
                (int-jump prog (foldl (lambda (a b) (int-assn b a)) st assns) jump)])))

(define int-jump
  (lambda (prog st jump)
    (match jump [`(goto ,label) 
                  (int-bb prog st (bb-lookup prog label))]
                [`(if ,cond ,then ,else) 
                  (int-bb prog st (bb-lookup prog (if (eval-exp st cond) then else)))]
                [`(return ,expr)
                  (eval-exp st expr)])))

(define (int-assn st assn)
    (match assn [`(:= ,var ,expr) (st-set st var (eval-exp st expr))]))

(define int-TM
  '((read Q Right)
    (init (:= Qtail Q)
          (:= Left '())
          (goto loop))
    (loop (if (null? Qtail) stop cont))
    (cont (:= Inst (cdar Qtail))
          (:= Qtail (cdr Qtail))
          (:= Ins (car Inst))
          (if (equal? Ins 'right) do-right cont1))
    (cont1 (if (equal? Ins 'left) do-left cont2))
    (cont2 (if (equal? Ins 'write) do-write cont3))
    (cont3 (if (equal? Ins 'goto) do-goto cont4))
    (cont4 (if (equal? Ins 'if) do-if error))
    (do-right (:= Left (cons (Car Right) Left))
              (:= Right (Cdr Right))
              (goto loop))
    (do-left (:= Right (cons (Car Left) Right))
            (:= Left (Cdr Left))
            (goto loop))
    (do-write (:= Symbol (cadr Inst))
              (:= Right (cons Symbol (Cdr Right)))
              (goto loop))
    (do-goto (:= Next-label (cadr Inst))
             (goto jump))
    (do-if (:= Symbol (cadr Inst))
           (:= Next-label (cadddr Inst))
           (if (equal? Symbol (Car Right)) jump loop))
    (jump (:= Qtail (new-Qtail Q Next-label))
          (goto loop))
    (error (return (append '(unknown instruction) Inst)))
    (stop (return Right))))