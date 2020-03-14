#lang racket

; Set is an unordered list

(define set (list 'a 'b 'c))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(element-of-set? 'x set)

(element-of-set? 'b set)

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

(adjoin-set 'c set)
(adjoin-set 'x set)

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

;Exercise 2.59: Implement the union-set operation
;for the unordered-list representation of sets.

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

(define (union-set set1 set2)
  (local
   ((define inters (intersetion-set set1 set2))
    (define set1-not-in-inter (not-in set1 inters))
    (define set2-not-in-inter (not-in set2 inters)))
    (append set1-not-in-inter inters set2-not-in-inter)))

set1
set2
(union-set set1 set2)





     