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

;Question-2
(check-run
;need to make sure that we are applying after evealuting all assignemnts

  ;; simple let
  (simple-let-x "let x=3,y=4 in x" 3)
  (simple-let-y "let x=3,y=4 in y" 4)
 
  ;; make sure the body and rhs get evaluated
  (eval-let-body "let x=3,y=5 in -(x,y)" -2)
  (eval-let-rhs "let x=-(4,1),y=0,z=1 in -(x,z)" 2)
 
  ;; check nested let and shadowing
  (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
  (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
  (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

  ;check that expresions are evaluated before env is added to
  ;remember that the default env is
  ;((i #(struct:num-val 1)) (v #(struct:num-val 5)) (x #(struct:num-val 10)))
  ;so it would be a shame if you used x and then spent a while figuring it out
  (no-val-in-env "let z=1,y=z in y" error)
  
)

;Question-3
(check-run
;need to make sure that we are applying after evealuting all assignemnts

  ;; simple let
  (simple-let-x "let* x=3,y=4 in x" 3)
  (simple-let-y "let* x=3,y=4 in y" 4)
 
  ;; make sure the body and rhs get evaluated
  (eval-let-body "let* x=3,y=5 in -(x,y)" -2)
  (eval-let-rhs "let* x=-(4,1),y=0,z=1 in -(x,z)" 2)
 
  ;; check nested let and shadowing
  (simple-nested-let "let* x = 3 in let y = 4 in -(x,y)" -1)
  (check-shadowing-in-body "let* x = 3 in let x = 4 in x" 4)
  (check-shadowing-in-rhs "let* x = 3 in let x = -(x,1) in x" 2)

  ;check that expresions are evaluated as we go
  (simple-let-y=z "let* z=1,y=z in y" 1)
  (eval-let-body "let* x=3,y=5,a=x,b=y in -(a,b)" -2)
  (eval-let-rhs "let* a=4,b=1,x=-(a,b),y=0,z=b in -(x,z)" 2)
  (simple-nested-let "let* a=3,x=a in let y = 4 in -(x,y)" -1)
  (check-shadowing-in-body "let x = 3 in let* x = 4 in x" 4)
  (check-shadowing-in-rhs "let x = 3 in let* x = -(x,1) in x" 2)
  (check-order "let* a=b,b=1 in a" error)

  
)
