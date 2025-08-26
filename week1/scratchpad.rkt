#lang racket
(require "cs514-useful-1.rkt")

;;; question 1
;;; Think of five different atoms and write them down. 
;;; And then build 10 different lists from those atoms,
;;; excluding the empty list.
(printf "question 1\n")


(define memberList?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
        (memberList? a (cdr lat)))))))

(define updateUniq 
    (lambda (cand collect)
      (cond
        [(null? cand) collect]
        [else
          (if (memberList? cand collect)
            collect
            (cons cand collect))]
    )))

(define shuffle
  (lambda (atoms collection)
    (cond
      [(null? atoms) collection]
      [else
       (shuffle (cdr atoms)
        (updateUniq (cdr (reverse atoms)) ;;;add reverse atoms -n[0] to collection
         (updateUniq (reverse atoms) ;;;add revers atoms to collection
          (updateUniq atoms collection)))) ;;;add current atoms to collection & pass to shuffle
       ])))

(shuffle '(1 2 3 4 5) '())

;;; question 2
;;; The list (all these problems) can be constructed by the form 
;;; (cons a (cons b (cons c d))) where
;;; a = all
;;; b = these
;;; c = problems
;;; d = ()
;;; construct the following lists
;;; (all (these problems))
;;; (all (these) problems)
;;; ((all these) problems)
;;; ((all) these problems)
;;; ((all these problems))
;;; the key is that in (cons x y), if y is a list then the result is a list
(printf "\nquestion 2\n")

(define a 'all)
(define b 'these)
(define c 'problems)
(define d (quote ()))

(cons a (cons b (cons c d)))

(cons a (cons (cons b (cons c d )) d))
(cons a (cons (cons b d) (cons c d )))
(cons (cons a (cons b d)) (cons c d ))
(cons (cons a d) (cons b (cons c d)))
(cons (cons a (cons b (cons c d))) d)

;;; question 3
;;;What is the (car (cons a l)) where a is french and ‘l’ is (fries)?
(printf "\nquestion 3\n")

(define A "french")
(define l 'fries)
(car (cons A l))

;;; question 4
;;; If a is an atom, is there a list that makes (null? (cons a l)) true?

(printf "\nquestion 4\n")

(define B "a")
(define ll null)
(null? (cons B ll))

;;; question 5
;;; Which of the following returns true (i.e., #t)?
;;;   (atom? (car ((meatballs) and spaghetti)))
;;;   (null? (cdr ((meatballs))))
;;;   (eq? (car l) (car (cdr l))) where l is (two meatballs)
(printf "\nquestion 5\n")

(atom? (car '('("meat") "&" "spag")))
(null? (cdr '('("meat"))))
(define l2 '("meat" "meat")) 
(eq? (car l2) (car (cdr l2)))
(eq? "meat" "meat")

(printf "in a var\n")
(define s1 "meat")
(define s2 "meat")
(eq? s1 s2) 

(printf "copy of the var\n")
(define s3 (string-copy s1))
(eq? s1 s3) 

;;; question 6
;;; Most LISP dialects have an if form that given (if ans-exp true-exp false-exp)
;;; will be texp when aexp is #t and fexp is false.
;;; Rewrite the following expression to use if:
;;;(cond 
;;;   ((null? l) nil)
;;;   (t (or 
;;;        (eq? (car l) a)
;;;        (member? a (cdr l)))))

(printf "\nquestion 6\n")
(set! l '("texp"))
(set! a '("aexp"))

(if (null? l) void (or (eq? (car l) a) (member? a (cdr l))) )

;;; question 7
;;; Write a function nonlat? that determines whether a list of S-expressions
;;; does not contain atomic S-expressions.

(printf "\nquestion 7\n")


(define nonLatEasy?
  (lambda (l)
    (not (lat? l))
  ))

(nonLatEasy? '("A" "B"))
(nonLatEasy? '())
(nonLatEasy? '('()))
(nonLatEasy? '('("a")))
(nonLatEasy? '("A" '("a")))

(printf "for real this time\n")

(define nonLat?
  (lambda (l)
    (cond
      [(null? l) #f] ;an empty list contains no non-atom elements but... see below
      [(not(atom? (car l))) #t] ; non atom, we're done
      [else (nonLat? (cdr l))] ; check the next one
    )))

(nonLat? '("A" "B"))
(nonLat? '())
(nonLat? '('()))
(nonLat? '('("a")))
(nonLat? '("A" '("a")))

(printf "strict nonlat\n")

(define strictNonLat?
  (lambda (l)
    (cond
      [(null? l) #t] ;an empty list contains no non-atom elements
      [(not (atom? (car l))) (strictNonLat? (cdr l))] ; non atom, check next
      [else #f] ; thats the list
    )))

(strictNonLat? '("A" "B"))  ;f
(strictNonLat? '())         ;t
(strictNonLat? '('()))      ;t
(strictNonLat? '('("a")))   ;t
(strictNonLat? '("A" '("a"))) ;f

;;; question 8
;;; The predicate member? tells whether some atom appears at least once in a lat.
;;; Write a predicate member-twice? that tells whether some atom appears at least 
;;; twice in a list of atoms.

(printf "\nquestion 8\n")

(define once '("A"))
(define twice '("A" "A"))
(define nonce '("b"))
(define nonsense '("N" "O" "N" "S" "E" "N" "S" "E"))
(set! a "A") ;remember we all point to the same string

(define member-twice?
  (lambda (a l)
    (cond
      [(null? l) #f]
      [else (if (not (member? a l)) #f (member? a (rember a l))) ]
    )))

(member-twice? a once)
(member-twice? a twice)
(member-twice? a nonce)
(member-twice? "N" nonsense)
(member-twice? "S" nonsense)
(member-twice? "E" nonsense)

;;; question 9
;;; Write a function seconds, which is a list of lats and makes a new lat
;;; consisting of the second atom from each lat is the list.
(printf "\nquestion 9\n")

(define ele2
  (lambda (l)
    (if (null? l) '() (car (cdr l))
  )))

(define seconds
  (lambda (l)
    (cond
      [(null? l) '()]
      [(not(pair? (car l))) '()]
      [else (cons (ele2 (car l)) (seconds (cdr l)))]
  )))


(define lll '(("A" "B" "C") ("a" "b" "c") ("1" "2" "3")))
(set! ll '((1 2 3) (1 2 3)))

(seconds lll)
(seconds ll)
(seconds '())
(seconds '(1 2))



