#lang racket
;;;
;;; File:   cs514-useful.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Take the code we built in the basics of Racket and use it to build a
;;; Racket package that we can use in other cases.
;;;

;;; We have to tell Racket which symbols in this package will be
;;; exported to other packages.   We're being lazy here and saying that
;;; we're exporting everything.   This is considered somewhat gauche by
;;; many Racket programmers but Your Glorious Instructor wasn't feeling
;;; up to keeping track of it all.
(provide (all-defined-out))

;;; Here's where TDD and the unit test framework come into play.  We
;;; want to unit test the smallest executable unit so we need to provide
;;; tests here that are independent of everything else.
(require rackunit)

;;; Useful functions:
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; lat: list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;;; And now back to our program
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))
(define l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(require rackunit)
(check-equal? (lat? l) #f)
;;;
;;; Note how this function implements the Fourth Commandment:
;;;
(define insertR*
  (lambda ( new old l)
    (cond
      ( ( null? l) (quote ()) )
      ( ( atom? ( car l))
        (cond
          (( eq? ( car l) old )
           ( cons old
                  ( cons new
                         ( insertR* new old
                                    (cdr l)))))
          (else (cons (car l)
                      (insertR* new old
                                (cdr l))))))
      (else (cons (insertR* new old
                            ( car l))
                  (insertR* new old
                            (cdr l)))))))

(define a 'banana)
(define aList '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
       (else (+ (occur* a (car l))
                (occur* a (cdr l))
                )))))

(check-equal? (occur* a aList) 5)

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
       (else
        (cons (subst* new old (car l))
              (subst* new old (cdr l)))))))

;;;
;;; It's all a matter of asking a question on each conditional
;;;
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? 2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))

(check-equal? (eqlist? '(1 2 3) '(1 2 3)) (eqlist2? '(1 2 3) '(1 2 3)))
;;;
;;; So, let's rewrite remove member function to use S-expressions
;;; The function should now remove the first matching S-exp s in l, instead of
;;; the first matching atom a in lat
;;;
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (rember s (cdr l)))))))))

; The firsts function builds a list of first s-expressions
;
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (cons (car (car l)) (firsts (cdr l)))))))

; Examples of firsts
;
(check-equal? (firsts '((apple peach pumpkin)
                        (plum pear cherry)
                        (grape raisin pea)
                        (bean carrot eggplant)))
               '(apple plum grape bean))

(check-equal? (firsts '((a b) (c d) (e f)))  '(a c e))
(check-equal? (firsts '((five plums) (four) (eleven green oranges)))
              '(five four eleven))
(check-equal? (firsts '(((five plums) four)
                        (eleven green oranges)
                        ((no) more)))
               '((five plums) eleven (no)))

;;; Helper functions for working with pairs

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
