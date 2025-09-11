#lang eopl
(require eopl/tests/private/utils)

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

;Question 4
(check-run
  (sanity-check "let x=1 in x" 1)
  
  (proc-simple-1 "let p = proc (val) zero?(val) in (p 1)" #f)
  (proc-simple-2 "let x=1 in let p = proc (val) zero?(val) in (p x)" #f)
  (proc-not-simple "let x=1 in let y=2 in let p = proc (val) -(x, -(y, val)) in (p 2)" 1)

  (simplified-book-sample  "let a=5 in
                              let p = proc (x) -(a,0) in
                                let a = 0 in
                                  (p -1)" 0)

  (book-sample "let a = 3 in
                  let p = proc (x) -(x,a) in
                      let a = 5 in
                        -(a, (p 2))" 8) ;-(5, -(2,5)) -> -(5, -3) -> 8
)
