#lang eopl
;;;
;;; File:      top.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Top level file for the proc interpreter
;;;
(require "data-structures.rkt")
(require "lang.rkt")
(require "interp.rkt")

(provide run)

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
