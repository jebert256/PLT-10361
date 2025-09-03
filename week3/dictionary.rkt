#lang racket

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;empty-env : () → Env
(define empty-env
  (lambda () (make-hash)))

;extend-env : Var × SchemeVal × Env → Env

;apply-env : Env × Var → SchemeVal

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env) (dict-empty? env)))

;has-binding? : Env x Var -> Boolean

(define empty (empty-env))

(require rackunit)

(check-pred dict? empty)

(check-true (empty-env? empty))