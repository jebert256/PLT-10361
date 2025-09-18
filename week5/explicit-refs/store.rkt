#lang eopl
(require racket/base)

(provide initialize-store! reference? newref deref setref!
         instrument-newref get-store-as-list)

(define instrument-newref (make-parameter #f))

;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

;; empty-store : () -> Sto
;; Page: 111
(define empty-store
  (lambda () (make-vector 0)))

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
;; Page 111
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; reference? : SchemeVal -> Bool
;; Page: 111
(define reference?
  (lambda (v)
    (integer? v)))

;; newref : ExpVal -> Ref
;; Page: 111
(define newref
  (lambda (val)
    (let ((next-ref (vector-length the-store))) ;copy len
      (let ((new-store (extend-store! the-store next-ref 1))) ;grow store
        (vector-set! new-store next-ref val) ;set value
        (set! the-store new-store) ;set value
      (when (instrument-newref)
        (eopl:printf 
         "newref: allocating location ~s with initial contents ~s~%"
         next-ref val))
      next-ref))
))

;; deref : Ref -> ExpVal
;; Page 111
(define deref 
  (lambda (ref)
    (vector-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; Page: 112
(define setref!                       
  (lambda (ref val)
    (if (< ref (vector-length the-store))
      (vector-set! the-store ref val)
      (report-invalid-reference ref the-store))
))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))
                
;; extend-store! : vector * int -> vector
(define extend-store!
  (lambda (old-store old-size n)
    (let ((new-store (make-vector (+ old-size n)))) ;grow by n
        (for ([i (in-range old-size)]) ;copy
          (vector-set! new-store i (vector-ref old-store i)))
        new-store)
))

(define get-store-as-list
  (lambda ()
    (let ((li '()))
      (for ([i (in-range the-store)])
        (cons li ('(i (vector-ref the-store i)))))
      li)
))
