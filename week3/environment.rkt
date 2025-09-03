#lang racket

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;empty-env : () → Env
(define empty-env
    (lambda () (list 'empty-env)))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
    (lambda (var val env)
        (list 'extend-env var val env)))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
        (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
        (let ((saved-var (cadr env))
          (saved-val (caddr env))
          (saved-env (cadddr env)))
            (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var)))
      ]
      [else (report-invalid-env env)])))
            
(define report-no-binding-found
    (lambda (search-var)
        (error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
    (lambda (env)
        (error 'apply-env "Bad environment: ~s" env)))

 ;code above this is copied from text
 ;question 2 code starts below
 ;===================================

;both of these are observers into the env

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env)
    (cond
      [(null? env) #f]
      [(and (list? env) (eq? (car env) 'empty-env)) #t]
      [else #f]
    )))

;has-binding? : Env x Var -> Boolean
(define has-binding?
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env) #f]
      [(eqv? (car env) 'extend-env)
        (let ((saved-var (cadr env))
          (saved-env (cadddr env)))
            (if (eqv? search-var saved-var)
              #t
              (has-binding? saved-env search-var)))
      ]
      [else (report-invalid-env env)])))

(require rackunit)

(define empty (empty-env))
(define cba (extend-env 'c 5 (extend-env 'b 4 (extend-env 'a 3 empty))))
(define acba (extend-env 'a 22 cba))

;test empty-env? should not throw error for bad data
(check-true (empty-env? empty))
(check-false (empty-env? cba))
(check-false (empty-env? '()))
(check-false (empty-env? 'bob))

;test has-binding? same as lookups
(check-true (has-binding? cba 'c))
(check-true  (has-binding? cba 'a))
(check-true  (has-binding? acba 'a))
(check-false  (has-binding? acba 'test-var))
(check-false  (has-binding? empty 'test-var))