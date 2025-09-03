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

(define report-no-binding-found
    (lambda (search-var)
        (error 'apply-env "No binding for ~s" search-var)))


(define empty (empty-env))
(define cba (extend-env 'c 5 (extend-env 'b 4 (extend-env 'a 3 empty))))
(define acba (extend-env 'a 22 cba))

(println empty)
(displayln cba)
(displayln acba)

(displayln (apply-env cba 'c))
(displayln (apply-env cba 'a))
(displayln (apply-env acba 'a))

