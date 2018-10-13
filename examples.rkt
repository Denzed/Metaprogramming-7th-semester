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
(cond [test-int (test "int(find, x)"                  (int find_name '(x (x y z) (1 2 3))) 1)
                (test "int(find, z)"                  (int find_name '(z (x y z) (1 2 3))) 3)
                (test "int(int-TM, [TM-prog, TM-in])" (int int-TM `(,TM-prog ,TM-in)) TM-out)]
      [else "skipping FlowChart interpreter tests"])

; mix tests
(cond [test-mix (let ([TM-FC-prog 
                       (do-mix int-TM 
                               (list->set '(Q Qtail Inst Ins Symbol Next-label))
                               (st-set st-empty 'Q TM-prog))])
                  ;;;    (pretty-print TM-FC-prog (current-error-port))
                     (test "mix(int-TM, TM-prog)(TM-in)" (int TM-FC-prog `(,TM-in)) TM-out))
                (let* ([compiler-division 
                        '(program division program-point-cur bb-index Inst)]
                       [compiler-prog
                        (do-mix 
                                mix
                                (list->set compiler-division)
                                (st-set* st-empty 
                                        'program  int-TM
                                        'division (list->set '(Q Qtail Inst Ins Symbol Next-label))))]
                       [TM-FC-prog 
                        (int compiler-prog `(,(st-set st-empty 'Q TM-prog)))])
                  ;;;     (pretty-print compiler-prog (current-error-port))
                  ;;;     (pretty-print TM-FC-prog (current-error-port))
                      (test "mix(mix, int-TM)(TM-prog)(TM-in)" (int TM-FC-prog `(,TM-in)) TM-out))]
      [else "skipping mix tests"])