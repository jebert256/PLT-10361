#lang eopl
(require eopl/tests/private/utils)
(require racket/base)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;Question 1

(check-run
    (simple-begin-let "equal? (begin let x=3 in x end, 3)" #t)
    (simple-begin-zero "equal? (begin zero?(0); -(4,1) end, 3)" #t)
    
    (begin-bindings "equal? (begin let x=3 in x; let y=55 in y end, 55)" #t)
    (begin-print "zero? (begin let x=55 in print(x) end)" #t)
    (begin-branch-true "equal? (if zero?(0) then
                        begin
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let x=55 in x
                        end
                        else
                        begin
                          let* x=-7000,y=350,z=-7 in print(-(x, -(z, y)));
                          let* x=-7000,y=350,z=-7 in print(-(x, -(z, y)));
                          let* x=-7000,y=350,z=-7 in print(-(x, -(z, y)))
                        end
                    , 55)" #t)
    (begin-branch-true "equal? (if zero?(1) then
                        begin
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let* x=7000,y=-350,z=7 in print(-(x,-(y,z)));
                          let x=55 in x
                        end
                        else
                        begin
                          let* x=-7000,y=350,z=-7 in print(-(x,-(y,z)));
                          let* x=-7000,y=350,z=-7 in print(-(x,-(y,z)));
                          let* x=-7000,y=350,z=-7 in print(-(x,-(y,z)))
                        end
                    , 0)" #t)
)

;Question 2
(check-run

)