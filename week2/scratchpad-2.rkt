#lang eopl
(require racket/base)


; tat => '(' atom tat tat ')' | '(' ')'
(define-datatype dll dll?
  [null-val (val (lambda (x) (null? x)))]
  [seq-val (x integer?) (left list?) (right list?)]
  
)


(define (number->sequence x)
  (seq-val x '() '()))

(define (list->seq l)
  (if (null? l)
    (error "cannot accept empty lists")
    (if (dll? l) 
      l
      (seq-val (car l) (car(cdr l)) (car (cdr(cdr l)))
  ))))



(define (current-element seq)
  (begin
    (set! seq (list->seq seq))
    (cases dll seq
      {null-val (val) void}
      {seq-val (x l r) x}
    )))

(define (move-to-left seq)
  (begin
    (set! seq (list->seq seq))
    (cases dll seq
      {null-val (val) seq}
      {seq-val (x l r) (if (at-left-end? seq) seq (seq-val (car l) (cdr l) (cons x r)))
      }
  )))

(define (move-to-right seq)
  (begin
    (set! seq (list->seq seq))
    (cases dll seq
      {null-val (val) seq}
      {seq-val (x l r) 
        (if (at-right-end? seq) seq (seq-val (car r) (cons x l) (cdr r)))
      }
    )))

(define (insert-to-left val seq)
  (if (null? seq)
    (number->sequence val)
    (begin
      (set! seq (list->seq seq))
      (cases dll seq
        {null-val (v) (number->sequence val)}
        {seq-val (x l r) (seq-val x (cons val l) r)}
      ))))


(define (insert-to-right val seq)
  (if (null? seq)
    (number->sequence val)
    (begin
      (set! seq (list->seq seq))
      (cases dll seq
        {null-val (v) (number->sequence val)}
        {seq-val (x l r) (seq-val x l (cons val r))}
      ))))

(define (at-left-end? seq)
  (begin
    (set! seq (list->seq seq))
    (cases dll seq
      {null-val (val) #t}
      {seq-val (x l r) (null? l)}
    )))

(define (at-right-end? seq)
  (begin
    (set! seq (list->seq seq))
    (cases dll seq
      {null-val (val) #t}
      {seq-val (x l r) (null? r)}
    )))


(displayln (number->sequence 7))
(displayln (current-element '(6 (5 4 3 2 1) (7 8 9))))
(displayln (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
(displayln (move-to-right '(6 (5 4 3 2 1) (7 8 9))))
(displayln (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))
(displayln (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))

(displayln "edges")
(displayln (list->seq '(6 (5 4 3 2 1) (7 8 9))))
(displayln (move-to-left '(6 () (7 8 9))))
(displayln (move-to-right '(6 (5 4 3 2 1) ())))
(displayln (insert-to-left 13 '()))
(displayln (insert-to-right 13 '()))

;;following will trigger the list->seq erorr
;;(displayln (current-element '()))
;;(displayln (move-to-left '()))
;;(displayln (move-to-right '()))