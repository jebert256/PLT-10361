#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require racket/base)

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

      (let-exp (idents exps body)
        (value-of body (pre-eval-env-update idents (evaluate-all exps env) env))
      )

      (let-star-exp (idents exps body)
        (value-of body (simple-env-update idents exps env))
      )

      ;=========
      ;additions
      ;=========

      (begin-exp (exps)
        (begin 
          ;(displayln exps)
        (evaluate-begin exps env)
        )
      )

      (print-exp (exp1)
        (begin
          (println (value-of exp1 env))
          (num-val 0)
        ))
)))


;pre-eval-env-update : var * Exp * Env-> Env
(define pre-eval-env-update 
  (lambda (idents vals env) 
    (cond
      [(null? idents) env]
      [else
        (let*
          ((var (car idents))
          (val (car vals)))
        (pre-eval-env-update (cdr idents) (cdr vals) (extend-env var val env)))
      ])))

;simple-env-update : var * Exp * Env-> Env
(define simple-env-update
  (lambda (idents expressions env) 
    (cond
      [(null? idents) env]
      [else
        (let*
          ((var (car idents))
          (val (value-of (car expressions) env)))
        (simple-env-update (cdr idents) (cdr expressions) (extend-env var val env)))
      ])))

;evaluate-all : '(Exp) * Env-> '(ExpVal)
(define evaluate-all
  (lambda (expressions env)
    (cond
      [(null? expressions) '()]
      [else
        (cons (value-of (car expressions) env) (evaluate-all (cdr expressions) env))
      ]
    )))

;=========
;additions
;=========

(define evaluate-begin
  (lambda (expressions env)
    (cond
      [(null? (cdr expressions)) (value-of (car expressions) env)]
      [else 
        (begin
          (value-of (car expressions) env)
          (evaluate-begin (cdr expressions) env )
        )
      ]
    )))
(define evaluate-begin2
  (lambda (expressions env)
    (cond
      [(null? (cdr expressions))
       (value-of (car expressions) env)] ; final value
      [else
       (let ([val (value-of (car expressions) env)])
         ;; If value-of returns an updated env, you need to extract it
         ;; But if it doesn't, you need to modify value-of to return both value and env
         (evaluate-begin2 (cdr expressions) env))])))

