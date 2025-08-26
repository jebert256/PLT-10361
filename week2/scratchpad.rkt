#lang eopl
(require racket/base)


; tat => '(' atom tat tat ')' | '(' ')'
(define-datatype tat tat?
  [node-val (atom atom?) (left tat?) (right tat?)]
  [null-val (val (lambda (x) (null? x)))]
)


; atom => number | symbol 
(define-datatype atom atom?
  [atom-val (val (lambda (x)(and (not (pair? x)) (not (null? x)))))]
)

(define (empty? x)
  (cases tat x
    {null-val (val) #t}
    {node-val (a l r) #f}
  )
)

;returns 0 if no children or empty
;1 for left child
;2 for right child
;3 for both children
(define (children? x)
  (cases tat x
    {null-val (val) 0}
    {node-val (a l r)
      (cond 
        [(and (not(empty? l)) (not(empty? r))) 3] ;;both full
        [(and (empty? l) (empty? r)) 0] ;;both empty
        [(empty? l) 2] ;;left empty so right child
        [else 1]       ;;left was full so left child
      )

    }
  )
)


(define a (atom-val "A"))
(define b (atom-val "B"))

(define c (atom-val "C"))
(define r (atom-val "R"))
(define l (atom-val "L"))

(define empty (null-val '()))
(define baren (node-val a empty empty))
(define left (node-val l baren empty))
(define right (node-val r empty baren))
(define alphabet (node-val a {node-val b empty empty} {node-val c empty empty}))

(displayln (children? empty))
(displayln (children? baren))
(displayln (children? left))
(displayln (children? right))
(displayln (children? alphabet))