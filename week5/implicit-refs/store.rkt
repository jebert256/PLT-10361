#lang eopl
(require racket/base)
(require "data-structures.rkt")

(provide initialize-store! reference? newref deref setref!
         instrument-newref get-store-as-list)

(define instrument-newref (make-parameter #f))

;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

;;more closley alligned with memeory because access becomes O(1)
;;and we no longer have to search the list
;;set allows us to change the location without rebuilding and extending
;;the store now forces us to move to a contigus area and copy, though we
;;can (maybe i'll do get to it) preallocate to reduce costs

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



;; newref : ExpVal -> Ref
;; Page: 111
(define newref
  (lambda (val)
  (begin
        ;(printf "newref: ")
    (let ((next-ref (vector-length the-store))) ;copy len
      (let ((new-store (extend-store! the-store next-ref 1))) ;grow store
        (begin
        ;(printf "{next-ref: ~s new-store:~s val: ~s}\n" next-ref new-store val)
        (vector-set! new-store next-ref val) ;set value
        (set! the-store new-store) ;set value
        )
      (when (instrument-newref)
        (eopl:printf 
         "newref: allocating location ~s with initial contents ~s~%"
         next-ref val))
      next-ref))
  )
))

;; deref : Ref -> ExpVal
;; Page 111
(define deref 
  (lambda (ref)
  (begin
        ;(printf "deref{ref: ~s the-store: ~s} \n" ref the-store)
    (cond
      [(expval? ref) ref]
      [else (vector-ref the-store ref)]
      
  ))
))

;; setref! : Ref * ExpVal -> Unspecified
;; Page: 112
(define setref!                       
  (lambda (ref val)
    (begin
        ;(printf "setref! {ref: ~s val: ~s}\n" ref val)
    (cond
      [(expval? ref) (report-immutable-assignment)]
      [else (if (< ref (vector-length the-store))
        (vector-set! the-store ref val)
        (report-invalid-reference ref the-store))]
    ))
))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

(define report-immutable-assignment
  (lambda ()
    (eopl:error 'setref
                "illegal assignment to immutable reference")))

(define get-store-as-list
  (lambda ()
    (let ((li '()))
      (for ([i (in-range the-store)])
        (cons li ('(i (vector-ref the-store i)))))
      li)
))

;;incase i get adventures
;;it woudl be nice to extend by say 8 and indipendintly track the last item
;;this way we don't pay the extend and copy cost for every new var
;;in a fuller implimentation you probably want to doubel you're store up to
;;some threshold and have a resonable starting number.
;; extend-store! : vector * int -> vector
(define extend-store!
  (lambda (old-store size n)
    (let ((new-store (make-vector (+ size n)))) ;grow by n
        (for ([i (in-range size)]) ;copy
          (vector-set! new-store i (vector-ref old-store i)))
        new-store)
))
