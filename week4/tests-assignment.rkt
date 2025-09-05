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

;=================================
;above is boilerplate from test
;below is tests for the assignement
;==================================

;Question 1
(check-run
  (if-equal-true "equal?(0,0)" #t)
  (if-equal-false "equal?(0,1)" #f)
  (if-equal-compound "equal?(-(1,1), 0)" #t)
  (equal-typecheck "-(equal?(0,0),1)" error)
  (if-equal "if equal?(0,0) then 3 else 4" 3)

  (if-less-true "less?(0,1)" #t)
  (if-less-false "less?(0,0)" #f)
  (if-less-compound "less?(-(1,2), 0)" #t)
  (less-typecheck "-(less?(1,0),1)" error)
  (if-less "if less?(0,1) then 3 else 4" 3)

  (if-greater-true "greater?(1,0)" #t)
  (if-greater-false "greater?(0,0)" #f)
  (if-greater-compound "equal?(-(1,1), 0)" #t)
  (greater-typecheck  "-(greater?(1,0),1)" error)
  (if-greater "if greater?(-(11,1),0) then 3 else 4" 3)
)