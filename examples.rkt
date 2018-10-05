#lang racket

(require "int.rkt")
(require "mix.rkt")
(require "auxiliary_functions.rkt")

(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

; test
(define (test test-name val expected-val) 
      (printf "Test ~a: ~a\n" 
            test-name 
            (if (equal? val expected-val) 
                  "OK" 
                  (format "failed -- got ~v instead of ~v" val expected-val))))

(define test-int #t)
(define test-mix #t)

; interpreter tests
(define TM-prog '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(define TM-in '(1 1 0 1 1 0 1))
(define TM-out '(1 1 1 0 1))
(cond [test-int (test "find x" (int find_name '(x (x y z) (1 2 3))) 1)
                (test "find z" (int find_name '(z (x y z) (1 2 3))) 3)
                (test "int TM" (int int-TM `(,TM-prog ,TM-in)) TM-out)]
      [else "skipping FlowChart interpreter tests"])

; pretty printer for FlowChart programs
(define (print-prog prog) 
      (for-each 
            (lambda (bb) (cond [(equal? 'read (car bb)) (printf "~s\n" bb)]
                               [else (printf "(~s" (car bb))
                                     (map (lambda (bb-line) (printf "\n   ~s" bb-line)) (cdr bb))
                                     (printf ")\n")]))
            prog))

; convenient mix applier
(define (do-mix prog div sv)
      (let* ([div-in (list->set div)]
            [bound-pred (lambda (var) (st-bound? sv var))]
            [read-out (cons 'read (filter-not bound-pred (cdar prog)))])
           (cons read-out (int mix `(,prog ,div-in ,sv)))))

; mix tests
(cond [test-mix (let ([TM-FC-prog 
                       (do-mix int-TM 
                               '(Q Qtail Inst Ins Symbol Next-label)
                               (st-set st-empty 'Q TM-prog))])
                     (test "int TM-FC" (int TM-FC-prog `(,TM-in)) TM-out))
                (let ([compiler-prog
                       (do-mix mix
                               '(program division program-point_0)
                               (st-set (st-set st-empty 'program int-TM) 'division '(Q Qtail Inst Ins Symbol Next-label)))])
                     (print-prog compiler-prog))]
      [else "skipping mix tests"])