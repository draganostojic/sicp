#lang racket

(define show? true)

(define (show-descr1 show?)
  (local
    ((define descr
#<<here-string-descr

Exercise 2.40
Define a procedure unique-pairs that, given an integer n,
generates the sequence of pairs (i, j) with 1 ≤ j < i ≤ n.



here-string-descr
       ))
    (if show? (display descr) (void))))

(show-descr1 show?)

(define (unique-pairs n)
  (foldr append
         null
         (map (λ (i)
                (map (λ (j)
                       (cons i j))
                     (range 1 i)))
         (range 2 (+ n 1)))))

(define (test1 show?)
  (local
    ((define n 10)
     (define res (unique-pairs n)))
    (begin
     (display "Test1\n")
     (display "n ") (display n) (newline)
     (display "unique-pairs ") (display res) (newline))))

(test1 show?)

(define (show-descr2 show?)
  (local
    ((define descr
#<<here-string-descr

Exercise 2.41: Write a procedure to find all
ordered triples of distinct positive integers i, j, and k
less than or equal to a given integer n that sum
to a given integer s.



here-string-descr
       ))
    (if show? (display descr) (void))))

(show-descr2 show?)

(define (dist-tripl-sum n sum)
  (local
    ((define dist-tripl
       (foldr append null (map (λ (i)
                                (foldr append null (map (λ (j)
                                                         (map (λ (k)
                                                                (list i j k))
                                                              (range 1 j)))
                                                       (range 2 i))))
                              (range 3 (+ n 1)))))
     (define res
       (filter (λ (tripl)
                 (local
                   ((define i (car tripl))
                    (define j (car (cdr tripl)))
                    (define k (car (cdr (cdr tripl)))))
                   (= (+ i j k) sum)))
               dist-tripl)))
    (begin
;      (display "tripl\n")
;      (display dist-tripl) (newline)
;      (display "tripl that sum to ") (display sum) (newline)
;      (display res) (newline)
      res)))


(define (test2 show?)
  (local
    ((define name "Test2")
     (define n 10)
     (define sum 21)
     (define res (dist-tripl-sum n sum)))
    (begin
      (display name) (newline)
      (display "n ") (display n) (newline)
      (display "sum ") (display sum) (newline)
      (display "dist-tripl-sum ") (display res) (newline))))

(test2 show?)





         


