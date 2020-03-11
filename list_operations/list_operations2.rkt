#lang racket

; How to test list length?

; A data structure qualifies to be called a list if it's the following:

; empty list (variable null or value '())
; (list? (cdr (cons x y)) evalueates to true
; list is sequence of pairs such that cdr eventually ends with empty list

(define (list2? x)
  (cond
    ((null? x) true)
    ((pair? x) (list? (cdr x)))
    (else false)))

(display "list? tests\n")

(list2? (cons (list 1 2) (cons 3 4)))
(list2? (cons (list 1 2) (cons 3 null)))
(list2? (cons (list 1 2) (list 3 4)))
(list2? (cons 1 2))
(list2? (cons 1 '()))
(list2? (cons 1 (list 2 3)))

(define (length2 l)
  (local
    ((define (iterate res l)
       (cond
         ((null? l) res)
         ((pair? l) (iterate (+ res 1) (cdr l)))
         (else false))))
    (iterate 0 l)))

(display "length tests\n")

(length2 (cons (list 1 2) (cons 3 4)))
(length2 (cons (list 1 2) (cons 3 null)))
(length2 (cons (list 1 2) (list 3 4)))
(length2 (cons 1 2))
(length2 (cons 1 '()))
(length2 (cons 1 (list 2 3)))


