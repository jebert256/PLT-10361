#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "substitutions.rkt")
(require "unifier.rkt")

(provide type-of-program type-of)

(define-datatype answer answer?
  (an-answer (type type?)
             (subst substitution?)))

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record (sym symbol?)
                        (type type?)
                        (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record () (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
        (if (eqv? sym sym1) val1 (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
      (extend-tenv 'v (int-type)
        (extend-tenv 'i (int-type) empty-tenv)))))

(define new-tvar
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      (tvar-type id))))

(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (new-tvar))
      (a-type (ty) ty))))

(define gen-eq
  (lambda (exp tenv eqns)
    (cases expression exp
      (const-exp (num) (list (int-type) eqns))
      (zero?-exp (e)
        (let* ((res (gen-eq e tenv eqns))
               (ty1 (car res))
               (eqns1 (cadr res)))
          (list (bool-type) (cons (list ty1 (int-type) e) eqns1))))
      (diff-exp (e1 e2)
        (let* ((res1 (gen-eq e1 tenv eqns))
               (ty1 (car res1))
               (eqns1 (cadr res1))
               (res2 (gen-eq e2 tenv eqns1))
               (ty2 (car res2))
               (eqns2 (cadr res2)))
          (list (int-type)
                (append eqns2 (list (list ty1 (int-type) e1)
                                    (list ty2 (int-type) e2))))))
      (if-exp (cond-exp then-exp else-exp)
        (let* ((res1 (gen-eq cond-exp tenv eqns))
               (cond-ty (car res1))
               (eqns1 (cadr res1))
               (res2 (gen-eq then-exp tenv eqns1))
               (then-ty (car res2))
               (eqns2 (cadr res2))
               (res3 (gen-eq else-exp tenv eqns2))
               (else-ty (car res3))
               (eqns3 (cadr res3)))
          (list then-ty
                (append eqns3 (list (list cond-ty (bool-type) cond-exp)
                                    (list then-ty else-ty exp))))))
      (var-exp (v) (list (apply-tenv tenv v) eqns))
      (let-exp (v rhs body)
        (let* ((res1 (gen-eq rhs tenv eqns))
               (rhs-ty (car res1))
               (eqns1 (cadr res1)))
          (gen-eq body (extend-tenv v rhs-ty tenv) eqns1)))
      (proc-exp (v otype body)
        (let* ((arg-type (otype->type otype))
               (res1 (gen-eq body (extend-tenv v arg-type tenv) eqns))
               (body-ty (car res1))
               (eqns1 (cadr res1)))
          (list (proc-type arg-type body-ty) eqns1)))
      (call-exp (rator rand)
        (let* ((result-type (new-tvar))
               (res1 (gen-eq rator tenv eqns))
               (rator-ty (car res1))
               (eqns1 (cadr res1))
               (res2 (gen-eq rand tenv eqns1))
               (rand-ty (car res2))
               (eqns2 (cadr res2)))
          (list result-type
                (append eqns2 (list (list rator-ty (proc-type rand-ty result-type) exp)))
      )))
      (letrec-exp (proc-result-otype proc-name bvar proc-arg-otype proc-body letrec-body)
        (let* ((proc-arg-ty (otype->type proc-arg-otype))
               (proc-result-ty (otype->type proc-result-otype))
               (tenv-letrec (extend-tenv proc-name (proc-type proc-arg-ty proc-result-ty) tenv))
               (tenv-body (extend-tenv bvar proc-arg-ty tenv-letrec))
               (res1 (gen-eq proc-body tenv-body eqns))
               (body-ty (car res1))
               (eqns1 (cadr res1)))
          (gen-eq letrec-body tenv-letrec
                              (append eqns1 (list (list body-ty proc-result-ty proc-body)))
          )))
)))

(define solve-eq
  (lambda (eqns)
    (letrec ((loop
              (lambda (eqns subst)
                (if (null? eqns)
                    subst
                    (let* ((eq (car eqns))
                           (t1 (car eq))
                           (t2 (cadr eq))
                           (exp (caddr eq))
                           (new-subst (unifier t1 t2 subst exp)))
                      (loop (cdr eqns) new-subst))))))
      (loop eqns (empty-subst)))))


(define type-of
  (lambda (exp tenv subst)
    (let* ((res (gen-eq exp tenv '()))
           (ty (car res))
           (eqns (cadr res))
           (final-subst (solve-eq eqns)))
      (apply-subst-to-type ty final-subst))))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (type-of exp1 (init-tenv) (empty-subst))))))
