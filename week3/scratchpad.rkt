#lang eopl
(require racket/base)


; tat => '(' atom tat tat ')' | '(' ')'
(define-datatype environment environment?
  [empty-env (val (lambda (x) (and (list? x) (null? x))))]
  [inst-env (var list?) (val list?)]
  
)



