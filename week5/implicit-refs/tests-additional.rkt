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

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run  
  (simple-let-1 "let a = 3 in a" 3)
  (eval-let-body "let x = 3 in -(x,1)" 2)
  (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)
  (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
  (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
  (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

   (assignment-test "let x = 17
                          in begin set x = 27; x end"
                    error)

)

