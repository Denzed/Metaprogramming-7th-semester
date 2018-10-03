#lang racket

(require "int.rkt")
(require "mix.rkt")
(require "auxiliary_functions.rkt")

(define test-int #t)
(define test-mix #t)

(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

; test
(define (test test-name val expected-val) (printf "Test ~a: ~a\n" test-name (if (equal? val expected-val) "OK" "failed")))

; interpreter tests
(define TM-prog '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(cond [test-int (test "find x" (int find_name '(x (x y z) (1 2 3))) 1)
                (test "find z" (int find_name '(z (x y z) (1 2 3))) 3)
                (test "int TM" (int int-TM `(,TM-prog (1 1 0 1 1 0 1))) '(1 1 1 0 1))]
      [else "skipping FlowChart interpreter tests"])

; mix tests
(cond [test-mix (int mix `(,int-TM (Q Qtail Inst Ins Symbol Next-label) ,(st-set st-empty 'Q TM-prog)))]
      [else "skipping mix tests"])