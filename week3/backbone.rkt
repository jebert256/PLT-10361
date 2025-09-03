#lang racket

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;empty-env : () → Env
(define empty-env
  (lambda () (list '() '())))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (list (cons var (car env)) (cons val (cadr env)))))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      [(null? (car env)) (report-no-binding-found search-var)]
      [(eq? (caar env) search-var) (caadr env)]
      [else (apply-env (list (cdar env) (cdadr env)) search-var)]
    )))

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env)
    (and
      (and 
        (list? env) ;;it's a list
        (not (null? env)) ;;it's not empty
      )
      (and 
        (list? (car env)) ;;it's a list
        (null? (car env)) ;;it iss empty
      ))))

;has-binding? : Env x Var -> Boolean
(define has-binding?
  (lambda (env search-var)
    (cond
      [(null? (car env)) #f]
      [(eq? (caar env) search-var) #t]
      [else (has-binding? (list (cdar env)) search-var)] ;ignore vals
    )))

(define report-no-binding-found
    (lambda (search-var)
        (error 'apply-env "No binding for ~s" search-var)))


(define empty (empty-env))
(define cba (extend-env 'c 5 (extend-env 'b 4 (extend-env 'a 3 empty))))
(define acba (extend-env 'a 22 cba))


(require rackunit)

;question 1
;==========

;test the empty-env and extend-env
(check-equal? empty '(() ()))
(check-equal? cba '((c b a) (5 4 3)))
(check-equal? acba '((a c b a) (22 5 4 3)))

;test the lookups
(check-equal? (apply-env cba 'c) 5)
(check-equal?  (apply-env cba 'a) 3)
(check-equal?  (apply-env acba 'a) 22)

;test the exceptions
(check-exn exn:fail?
  (lambda () (report-no-binding-found 'test-method)))
(check-exn exn:fail?
  (lambda () (apply-env cba 'test-var)))
(check-exn exn:fail?
  (lambda () (apply-env empty 'test-var)))

;question 2
;==========

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