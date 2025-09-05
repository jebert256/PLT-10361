#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (apply-env env var))
      
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;=========
      ;additions
      ;=========

      (equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (let ((val2 (value-of exp2 env)))
              (let ((num2 (expval->num val2)))
                (if (equal? num1 num2)
                         (bool-val #t)
                         (bool-val #f)
      ))))))

      (less?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (let ((val2 (value-of exp2 env)))
              (let ((num2 (expval->num val2)))
                (if (< num1 num2)
                         (bool-val #t)
                         (bool-val #f)
      ))))))

      (greater?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (let ((val2 (value-of exp2 env)))
              (let ((num2 (expval->num val2)))
                (if (> num1 num2)
                         (bool-val #t)
                         (bool-val #f)
      ))))))
      
)))

