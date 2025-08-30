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
))

(define (insert t at)
  (cases tat t
    {null-val (val) 
      (node-val at (null-val '()) (null-val '()))}
    {node-val (a l r) 
      (cases atom a
        {atom-val (a-val)
          (cases atom at
            {atom-val (at-val)
              (if (< at-val a-val) 
                (node-val a (insert l at) r) ;;left child
                (node-val a l (insert r at)) ;;right child
          )})
      })
}))

(define (member? t at)
  (cases tat t
    {null-val (val) #f}
    {node-val (a l r)
      (cases atom a
        {atom-val (a-val)
          (cases atom at
            {atom-val (at-val)
              (if (= a-val at-val) 
                #t ;;found it
                (if (< at-val a-val) 
                  (member? l at) ;;left child
                  (member? r at) ;;right child
          ))})
      })
}))


(define (appendTat t apende)
  (cases tat t
    {null-val (_) apende} ;;return the appende
    {node-val (a l r) 
      (cases atom a
        {atom-val (a-val)
          (cases tat apende
            {null-val (_) t} ;;return the original
            {node-val (ap-at ll rr) 
              (cases atom ap-at
                {atom-val (ap-val)
                  (if (< ap-val a-val);; do we go left or right
                    (appendTat apende t) ;;swap whos being inserted
                    (node-val a (appendTat l apende) r) ;;insert left
              )})
            }
          )})
      })
)


(define (delete t at)
  (cases tat t
    {null-val (val) #f}
    {node-val (a l r)
      (cases atom a
        {atom-val (a-val)
          (cases atom at
            {atom-val (at-val)
              (if (= a-val at-val) 
                ;;to remove the current node we add the right child 
                ;;to the left child and return the new sub tree
                (appendTat l r)
                (if (< at-val a-val) 
                  (node-val a (delete l at) r) ;;left child
                  (node-val a l (delete r at)) ;;right child
          ))})
      })
}))

(define (print-atom node)
  (cases tat node
    {null-val (val) (displayln "()")}
    {node-val (a l r) 
      (displayln a)}
  )
)

(define (atom++ node)
  (cases tat node
    {null-val (val) node}
    {node-val (a l r) 
      (cases atom a
        {atom-val (a-val)
          (node-val (atom-val (+ a-val 1)) l r)})}
  )
)

(define (pre-order t ex)
  (begin 
    (define new-t (ex t))
    (when (tat? new-t) ; update the node
      (set! t new-t))
    (cases tat t
      {null-val (val) t}
      {node-val (a l r) 
        (node-val a (pre-order l ex) (pre-order r ex))
    })
  )
)

(define (in-order t ex)
    (cases tat t
      {null-val (val) t}
      {node-val (a l r) 
        (begin
          (define newleft (pre-order l ex)) ;;travers left side
          (define new-t (ex (node-val a newleft r))) ;work current node
          (when (tat? new-t) ; update the node
            (set! t new-t))
          (cases tat t 
            {null-val (val) t}
            {node-val (a l r) 
              (node-val a l (in-order r ex)) ;;set final and traverse right
          })
        )
      }
))

(define (post-order t ex)
  (cases tat t
      {null-val (val) t}
      {node-val (a l r) 
        (begin
          (define newleft (post-order l ex)) ;;travers left side
          (define newRight (post-order r ex)) ;;travers right side
          (define new-t (ex (node-val a newleft newRight))) ;work current node
          (when (tat? new-t) ; update the node
            (set! t new-t))
          (cases tat t 
            {null-val (val) t}
            {node-val (a l r) t} ;;return the node
          )
        )
      }
  )
)

(define a (atom-val 1))
(define b (atom-val 2))

(define c (atom-val 3))
(define r (atom-val 18))
(define l (atom-val 12))

(define empty (null-val '()))
(define baren (node-val (atom-val 0) empty empty))
(define left (node-val l baren empty))
(define right (node-val r empty baren))
(define alphabet (node-val a {node-val b empty empty} {node-val c empty empty}))
(define extraAlpha (insert (insert alphabet r) l))


(displayln (insert baren c))
(displayln (member? alphabet c))
(displayln (member? baren c))
(displayln (appendTat alphabet baren))
(displayln (pre-order alphabet print-atom))
(displayln (pre-order alphabet atom++))
(in-order alphabet print-atom)
(post-order alphabet print-atom)