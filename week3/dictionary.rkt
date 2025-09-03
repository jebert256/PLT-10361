#lang racket

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;empty-env : () → Env
(define empty-env
  (lambda () (make-hash)))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (begin
        (define copy (hash-copy env)); new env
        (dict-set! copy var val) copy))) ;change and return

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env var)
    (dict-ref env var)))

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env) 
    (and 
      (hash? env) ;;protect from hash / dict differences
      (dict-empty? env)
    )))

;has-binding? : Env x Var -> Boolean
(define has-binding?
  (lambda (env var) 
    (and 
      (not (empty? env)) ;;protect dict contract violation
      (dict-has-key? env var)
    )))

(define empty (empty-env))
(define cba (extend-env 'c 5 (extend-env 'b 4 (extend-env 'a 3 empty))))
(define acba (extend-env 'a 22 cba))

(require rackunit)

;check we have a dict
(check-pred dict? empty)

;test the empty-env and extend-env
(check-equal? empty (make-hash))
(check-equal? cba (make-hash '((a . 3) (b . 4) (c . 5))))
(check-equal? acba (make-hash '((a . 22) (b . 4) (c . 5))))

;test the lookups
(check-equal? (apply-env cba 'c) 5)
(check-equal?  (apply-env cba 'a) 3)
(check-equal?  (apply-env acba 'a) 22)

;no custom exceptions in this one

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