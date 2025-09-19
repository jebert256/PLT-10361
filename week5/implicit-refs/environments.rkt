#lang eopl

(require "data-structures.rkt")
(require "store.rkt")
(require racket/base)
(provide init-env empty-env extend-env-mutable apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; (init-env) builds an environment in which:
;; i is bound to a location containing the expressed value 1, 
;; v is bound to a location containing the expressed value 5, and 
;; x is bound to a location containing the expressed value 10.  
(define init-env 
  (lambda ()
    (extend-env-mutable 
     'i (newref (num-val 1))
     (extend-env-mutable
      'v (newref (num-val 5))
      (extend-env-mutable
       'x (newref (num-val 10))
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-env-mutable (bvar bval saved-env)
                  (if (eqv? search-var bvar)
                      bval
                      (apply-env saved-env search-var)))
      (extend-env (bvar val saved-env)
                (begin
                ;(printf "apply-env.extend-env {bvar: ~s, val: ~s, search-var: ~s, \n\tsaved-env: ~s}\n"
                ;  bvar val search-var saved-env)
                  (if (eqv? search-var bvar)
                      val
                      (apply-env saved-env search-var)))
      )
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (let ((n (location search-var p-names)))
                         (begin
                ;(printf "extend-env-rec* {p-names ~s, b-vars ~s, p-bodies ~s, \n\tsaved-env: ~s}\n"
                ;  p-names b-vars p-bodies saved-env)
                         (if n
                             (newref
                              (proc-val
                               (procedure 
                                (list-ref b-vars n)
                                (list-ref p-bodies n)
                                env)))
                             (apply-env saved-env search-var))))))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))

