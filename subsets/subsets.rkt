#lang racket

; Exercise 2.32
; subs(S) = car(S) x subs(cdr(S)) U subs(cdr(S))
; NOTE: empty set must be represented as (list empty)
; because if it're represented as empty map will treat is as empty list and not apply
(define (subsets s)
  (cond
    ((null? s) (list '()))
    (else
     (local
       ((define ss-cdrs (subsets (cdr s)))
        (define (x y) (cons (car s) y))
        (define res (append (map x ss-cdrs) ss-cdrs)))
       
       res))))

(define test1
  (begin
    (display "Test1\n")
    (local
      ((define s (list 1 2 3)))
      (begin
        (display "S=") (display s) (newline)
        (display "Sub(S)=") (display (subsets s)) (newline)
        (newline)))))
                                     
test1

(define test2
  (begin
    (display "Test2\n")
    (local
      ((define s (list 1 2 3 4)))
      (begin
        (display "S=") (display s) (newline)
        (display "Sub(S)=") (display (subsets s)) (newline)
        (newline)))))
                                     
test2

(define test3
  (begin
    (display "Test3\n")
    (local
      ((define s (list 'a 'b 'c 'd 'e)))
      (begin
        (display "S=") (display s) (newline)
        (display "Sub(S)=") (display (subsets s)) (newline)
        (newline)))))
                                     
test3



