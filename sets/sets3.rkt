#lang racket

(define show? true)

((λ (show?)
   (local
     ((define descr
        #<<here-string
Sets as ordered lists

consider only the case where the set
elements are numbers, so that we can compare elements
using > and <. We will represent a set of numbers by listing
its elements in increasing order. Whereas our first repre-
sentation above allowed us to represent the set {1, 3, 6, 10}
by listing the elements in any order, our new representa-
tion allows only the list (1 3 6 10).
here-string
        ))
     (if (not show?)
         (void)
         (begin
           (display descr)
           (newline)
           (newline)))))
 show?)

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((< x (car set)) false)
    ((= x (car set)) true)
    (else (element-of-set? x (cdr set)))))

((λ (name show?)
   (cond
     ((not show?) (void))
     (else
      (begin
        (display name) (newline)
        (local
          ((define set (list 1 3 6 10))
           (define x1 5)
           (define x2 10))
          (begin
            (display "set ") (display set) (newline)
            (display "x1 ") (display x1) (newline)
            (display "x2 ") (display x2) (newline)
            (local
              ((define res1 (element-of-set? x1 set))
               (define res2 (element-of-set? x2 set)))
              (begin
                (display "x1 is elem of set ") (display res1) (newline)
                (display "x2 is elem of set ") (display res2) (newline)))
            (newline)
            (newline)))))))
 "Test1" show?)

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    (else
     (local
       ((define x2 (car set)))
       (cond
         ((< x x2) (cons x set))
         ((= x x2) set)
         (else (cons x2 (adjoin-set x (cdr set)))))))))

((λ (name show?)
   (cond
     ((not show?) (void))
     (else
      (begin
        (display name) (newline)
        (local
          ((define set (list 1 3 6 10))
           (define x1 4)
           (define x2 3)
           (define x3 -1)
           (define x4 25))
          (begin
            (display "set ") (display set) (newline)
            (display "x1 ") (display x1) (newline)
            (display "x2 ") (display x2) (newline)
            (display "x3 ") (display x3) (newline)
            (display "x4 ") (display x4) (newline)
            (local
              ((define res1 (adjoin-set x1 set))
               (define res2 (adjoin-set x2 set))
               (define res3 (adjoin-set x3 set))
               (define res4 (adjoin-set x4 set)))
              (begin
                (display x1) (display " added to set ") (display res1) (newline)
                (display x2) (display " added to set ") (display res2) (newline)
                (display x3) (display " added to set ") (display res3) (newline)
                (display x4) (display " added to set ") (display res4) (newline)))
            (newline)
            (newline)))))))
 "Test2" show?)

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) null)
    (else
     (local
       ((define x1 (car set1))
        (define x2 (car set2)))
       (cond
         ((= x1 x2)
          (cons x1 (intersection-set (cdr set1) (cdr set2))))
         ((< x1 x2) (intersection-set (cdr set1) set2))
         (else
          (intersection-set set1 (cdr set2))))))))

((λ (name show?)
   (cond
     ((not show?) (void))
     (else
      (begin
        (display name) (newline)
        (local
          ((define set1 (list 1 3 6 10))
           (define set2 (list 2 3 4 5 6)))
          (begin
            (display "set1 ") (display set1) (newline)
            (display "set2 ") (display set2) (newline)
            (local
              ((define res (intersection-set set1 set2)))
              (begin
                (display "intersection ") (display res) (newline)))
            (newline)
            (newline)))))))
 "Test3" show?)

; Exercise 2.62: Give a Θ(n) implementation of union-
; set for sets represented as ordered lists.

; I need to take all elements from set1 and insert all elements
; from set2 except those that are in x1

; Note that union-set that produces an ordered list from two
; ordered lists is a merge operation (without duplicates bc we're woring with sets)
(define (union-set set1 set2)
  (local
    ((define (iterate set1 set2 union)   
       (begin
         (display "set1 ") (display set1)
         (display " set2 ") (display set2)
         (display " union ") (display union)
         (newline)
         (cond
           ((and (null? set1)
                 (null? set2))
            union)
           ((or (null? set1)
                (null? set2))
                (iterate null null (append union (if (null? set1) set2 set1))))
           (else
            (local
              ((define x1 (car set1))
               (define x2 (car set2)))
              (cond
                ((= x1 x2) (iterate (cdr set1) (cdr set2) (append union (list x1))))
                ((< x1 x2) (iterate (cdr set1) set2 (append union (list x1))))
                (else (iterate set1 (cdr set2) (append union (list x2)))))))))))
    (iterate set1 set2 null)))


((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s1 (list 1 3 5 25 27))
            (define s2 (list -3 5 22 100 123)))
           (begin
             (display "s1 ") (display s1) (newline)
             (display "s2 ") (display s2) (newline)
             (local
               ((define res (union-set s1 s2)))
               (begin
                 (display "union ") (display res) (newline)))))
         (newline)
         (newline))))
 "Test4" show?)

; More efficient because it doesn't use append
(define (union-set2 s1 s2)
  (cond
    ((null? s1) s2)
    ((null? s2) s1)
    (else
     (local
       ((define x1 (car s1))
        (define x2 (car s2)))
       (cond
         ((= x1 x2) (cons x1 (union-set2 (cdr s1) (cdr s2))))
         ((< x1 x2) (cons x1 (union-set2 (cdr s1) s2)))
         (else (cons x2 (union-set2 s1 (cdr s2)))))))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s1 (list 1 3 5 25 27))
            (define s2 (list -3 5 22 100 123)))
           (begin
             (display "s1 ") (display s1) (newline)
             (display "s2 ") (display s2) (newline)
             (local
               ((define res (union-set2 s1 s2)))
               (begin
                 (display "union ") (display res) (newline)))))
         (newline)
         (newline))))
 "Test5" show?)
               
             





                         



