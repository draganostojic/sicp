#lang racket

;Exercise 2.60: We specified that a set would be rep-
;resented as a list with no duplicates. Now suppose
;we allow duplicates. For instance, the set {1, 2, 3}
;could be represented as the list (2 3 2 1 3 2 2).
;Design procedures element-of-set?, adjoin-set,
;union-set, and intersection-set that operate on
;this representation.

(define set (list 'a 'b 'c))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(element-of-set? 'x set)

(element-of-set? 'b set)

(define (adjoin-set x set) (cons x set))

(define set3 (adjoin-set 'c set))

set3

(element-of-set? 'x set3)

(define set4 (adjoin-set 'x set3))

set4

(element-of-set? 'x set4)

(define (intersetion-set set1 set2)
  (cond
    ((null? set1) null)
    (else
     (local
       ((define result (intersetion-set (cdr set1) set2)))
       (cond
         ((element-of-set? (car set1) set2) (append (list (car set1)) result))
         (else result))))))

(define set1 (list 'a 'b 'c 'd 'e 'f 'g))
(define set2 (list '1 '2 'c 'd '5 '6))

(intersetion-set set1 set2)

(define (not-in s not-s)
  (local
    ((define (iterate s)
       (cond
         ((null? s) null)
         (else
          (local
            ((define result (iterate (cdr s))))
            (if (element-of-set? (car s) not-s) result (cons (car s) result)))))))
    (iterate s)))

(define (union-set set1 set2) (append set1 set2))

set1
set2
(union-set set1 set2)








     