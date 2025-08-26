#lang racket
;;;
;;; File:   racketbasics.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Support code for working our way through the The Little Schemer with
;;; focus on basics, recursion (again), and car and cons.
;;;

;;; # Preliminaries:
;;; 1. Note the #lang macro at the head of the file.  Racket is
;;; extremely extensible  and that macro defines the language variant in
;;; use.  Here, we're working with the base language.
;;; 2. Note both the structure and style used for comments in LISP
;;; variants.
;;; 3. Let's talk data.
;;;    a. numbers are written in the usual way with the extension that
;;;       you can do both fractions (1/2) and complex numbers (1+2i).
;;;    b. Strings are delimited by double quotes and have similar
;;;       behavior to other languages.
;;;    c. Do note that Racket uses wide characters and supports entry
;;;       and printing of things like Greek letters and symbols
;;;    d. Booleans are represented by the special values #t and #f
;;;
;;; And now to dive into code. The Little Schemer was written to model
;;; a Socratic discussion between an expert and new programmer.  So, we
;;; will work our way through the text, folliwing the responses and
;;; looking at the code as we go.
;;; 

;;; # Thinking Recursively About Data
;;;
;;; We begin with the concept of an atom.  In LISP variants like Racket,
;;; an "atom" is a thing with value, represented in code by a quoted
;;; string representation.   If you're a Javascript programmer, this is what
;;; inspired Javascript's Symbol object, which is the same concept.
;;;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; Other points of interest:
;;; * We describe the `atom?` as being a form
;;; * The `define` form links an atom to some expression.
;;; * And we see our first `lambda` form, which was the inspiration for
;;;   lambda functions in C++.  More on that later.
;;; * Note the `and` form, which returns the logical and of the two
;;;   expressions
;;; * And note the convention of ending a name with '?'.  This indicates
;;;   the form is a predicate, a function that returns #t or #f


;;; Lists in Racket are denoted by parenthesis.   There is the classic
;;; joke in the LISP community that LISP is an acronym for "Lots of
;;; Insidious Stupid Parentheses".   Note how everything can be
;;; considered to a a list.
;;;
;;; lat: list of atoms
(define lat?
    (lambda (l)
      (cond
        ((null? l) #t)
        ((atom? (car l) (lat? (cdr l))))
        (else #f))))

;;; Here's where the recursion comes into play.  Recall the recursive
;;; definition of a list from data structures?  Almost all functions in
;;; Racket (and LISP variants, in general) operate on recursion.  In
;;; this case the predicate `lat?` uses the Racket `cond` form to test
;;; the three cases in the recursive definition. And `cond`? Think of it
;;; as a superpowered version of a `switch`.

;;; Now to find out if an atom is a member of a list
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

;;;
;;; ## Unit Testing In Racket
;;; We're using Racket's unit test framework, trying to match the contents
;;; of the book.  This incredibly useful and y'all need to look towards
;;; how to use this to do TDD with Racket
(require rackunit)
(check-equal? (member? 'meat '(meat and gravy)) #t "Test: Start")
(check-equal? (member? 'meat '(and meat and gravy)) #t)
(check-equal? (member? 'meat '(mashed potatoes and meat gravy)) #t)
(check-equal? (member? 'and '(and meat and gravy)) #t)
(check-not-equal? (member? 'dog '(meat and gravy)) #t)

;;; ## And now we play with removing items from a list.
;;; Let's review the
;;; basic Racket (and LISP) list operations.  `car` gets you the first
;;; thing in a list (can be atom or another list while `cdr` gets you
;;; the list that is the rest of the list.  `cons` combines its two
;;; arguments into a list

(define rember-bad
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember-bad a (cdr lat))))))))

(check-equal? (rember-bad 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
(check-not-equal? (rember-bad 'and '(bacon letttuce and tomato)) '(bacon lettuce tomato))

;; Let's try again...
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember-bad a (cdr lat)))))))))

(check-equal? (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
(check-not-equal? (rember 'and '(bacon letttuce and tomato)) '(bacon lettuce tomato))

;;; Well, maybe one more time.
(define remberV2
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (remberV2 a (cdr lat)))))))

(check-equal? (remberV2 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))

;;; # Cons The Magnificent
;;; 
;;; Recursion is our friend, so let's see what how we can go about
;;; using it to extract the first item from a list of lists of atoms.
;;;
;;; Here's where we have to consider the concept of "s-expression". The
;;; term means "symbolic expression" and is classically defined as an
;;; atom or an expression of the form `(x . y) where x and y are
;;; S-expressions. Note how in a LISP variant like Racket, both code and
;;; data are expressed in S-Expressions.
;;;
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))
;;;
;;; Note the pattern, described in TLS as "The Third Commandment": When
;;; building a list, describe the first typical element and then `cons`
;;; it onto the natural recursion of definition of the S-Expression.

(check-equal? (firsts '((a b) (c d) (e f))) '(a c e))

;;;
;;; ## A digression into equality:
;;; One must specify carefully as to what is meant by equality.   We
;;; have that issue in C++ and other OO languages.  For instance, does
;;; anyone in the room know the difference between the `==` and `===`
;;; operators in C++17 and C++20?
;;;
;;; For Racket, we have `eq?` which is a primitive that will check to
;;; see if two non-numeric atoms are the same:

(check-true (eq? (car '(Mary had a little lamb)) 'Mary))

;;;  Contrast this with `equal?`, which is `#t` i.f.f. they are `eqv?`,
;;;  unless specifying otherwise by a particular datatype.  `eqv?` just
;;;  does an `eq?` except for things like numbers and character types
;;;  that have additional requirements on "equality".  Can you name what
;;;  those might be?

(check-true (equal? 'yes 'yes))
(check-false (equal? 2 2.0))

;;; ## Let's try inserting something into a list
;;; This will do a right insert: insert to the right of a search item
;;; in a list.   Our first try isn't so good.
(define insertRBad
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (insertRBad new old (cdr lat)))))))))

(check-not-equal?
 (insertRBad 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with fudge topping for dessert))

;;; And now we fix.
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons old (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with fudge topping for dessert))

;;; Now how we do a left insert
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
                (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

(check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping fudge for dessert))

;;; How about substitution?
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq?(car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

(check-equal? (subst 'topping 'fudge '(ice cream with topping for dessert))
              '(ice cream with topping for dessert))

;;; Two possibilities?
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) o1)
               (cons new (cdr lat)))
              ((eq? (car lat) o2)
               (cons new (cdr lat)))
              (else (cons (car lat))
                    (subst2 new o1 o2 (cdr lat))))))))

(check-equal? (subst2 'vanilla
                      'chocolate
                      'banana
                      '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping)
              "Subst2 works")

;; And now to remove all.  Here we need a function that produces as a
;; final value the lat with all occurrences of an atom removed.
;;;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) a)
          (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))))

(check-equal? (multirember 'cup '(coffee cup tea and hick cup))
             '(coffee tea and hick))

;;; and to immediate right
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old)
               (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertR new old (cdr lat)))))))))

;;; or immediate left
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons new
                (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertL new old (cdr lat)))))))))
;;;
;;; And to do the same for subst.   Here, we can start to get a feel for
;;; The Fourth Commandment: Always change at least one argument on a
;;; recursion so that you move closer to termination.
;;;
;;; When dealing with `cdr` of lists, test termination using `null?`.
;;;
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))
