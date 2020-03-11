#lang racket

; Exercise 2.33

(define (accumulate2 f init seq)
  (cond
    ((null? seq) init)
    (else (f (car seq) (accumulate2 f init (cdr seq))))))

(define test1
  (begin
   (display "\nTest1\n")
   (local
     ((define seq (list 1 2 3 4)))
     (begin
      (display "seq ") (display seq) (newline)
      (display "acculuate + ") (display (accumulate2 + 0 seq)) (newline)
      (display "accumulate * ") (display (accumulate2 * 1 seq)) (newline)
      (newline)))))

test1

(define (map2 p sequence)
  (accumulate2
   (λ (x y) (cons ((p x) y)))
   null sequence))

(define test2
  (begin
   (display "\nTest2\n")
   (local
     ((define seq (list 1 2 3 4)))
     (begin
      (display "seq ") (display seq) (newline)
      (display "map *2 ") (display (map (λ (x) (* x 2)) seq)) (newline)
      (newline)))))

test2

(define (append2 seq1 seq2)
  (accumulate2 cons seq2 seq1))

(define test3
  (begin
   (display "\nTest3\n")
   (local
     ((define seq1 (list 1 2 3 4))
      (define seq2 (list 5 6 7 8)))
     (begin
      (display "seq1 ") (display seq1) (newline)
      (display "seq2 ") (display seq2) (newline)
      (display "append ") (display (append2 seq1 seq2)) (newline)
      (newline)))))

test3


(define (length2 seq)
  (accumulate2 (λ (x y) (+ 1 y)) 0 seq))

(define test4
  (begin
   (display "\nTest4\n")
   (local
     ((define seq (list 1 2 3 4 5 6 7 8)))
     (begin
      (display "seq ") (display seq) (newline)
      (display "length ") (display (length2 seq)) (newline)
      (newline)))))

test4

;Exercise 2.34
;
(define (poly x seq)
  (local
    ((define (shiftseq seq)
       (cond
         ((null? (cdr seq)) null)
         (else (cons (car seq) (shiftseq (cdr seq))))))
     (define (lastel seq)
       (cond
         ((null? (cdr seq)) (car seq))
         (else (lastel (cdr seq)))))
     (define lastel2 (lastel seq))
     (define shiftseq2 (shiftseq seq)))
    (accumulate2 (λ (ak-1 acc2) (+ (* acc2 x) ak-1)) lastel2 shiftseq2)))

(define test5
  (begin
   (display "\nTest5\n")
   (local
     ((define seq (list 1 3 0 5 0 1))
      (define x 2))
     (begin
      (display "poly ") (display seq) (newline)
      (display "poly(") (display x) (display ") ") (display (poly x seq)) (newline)
      (display "check ") (display (+ 1 (* 3 x) (* 5 x x x) (* x x x x x))) (newline)
      (newline)))))

test5

; Enumerate leaves with accumulator

(define (fringe tree)
  (accumulate2
   (λ (x y)
     (cond
       ((pair? x) (append (fringe x) y))
       (else (cons x y))))
   null
   tree))

(define test6
  (local
    ((define tree1
       (cons (list 1 2) (list 3 4)))
     (define tree2
       (list (list (list 1 2 3 19 283 38) 2 3 2) (list 2 3 (list 217 382 1827) 2 187 (list 2838))
        2 1 2 (list 2 (list 3 (list 3)) 23 2 1 238))))
    (begin
     (display "Test6\n")
     (display "tree1: ") (display tree1) (newline)
     (display "leaves ") (display (fringe tree1)) (newline)
     (display "tree2: ") (display tree2) (newline)
     (display "leaves ") (display (fringe tree2)) (newline)
     (newline))))

test6

; Exercise 2.35

(list 1 2 3 (list 3 4))
(cons 1 (cons 2 (cons 3 (cons (cons 3 (cons 4 null)) null))))

(define (count-leaves tree)
  (accumulate2
   (λ (x y)
     (cond
       ((pair? x) (+ (count-leaves x) y))
       (else (+ 1 y))))
   0
   tree))

(define test7
  (local
    ((define tree1
       (cons (list 1 2) (list 3 4)))
     (define tree2
       (list (list (list 1 2 3 19 283 38) 2 3 2) (list 2 3 (list 217 382 1827) 2 187 (list 2838))
        2 1 2 (list 2 (list 3 (list 3)) 23 2 1 238))))
    (begin
     (display "Test7\n")
     (display "tree1: ") (display tree1) (newline)
     (display "count leaves ") (display (count-leaves tree1)) (newline)
     (display "tree2: ") (display tree2) (newline)
     (display "count leaves ") (display (count-leaves tree2)) (newline)
     (newline))))

test7


; Exercise 2.36

(define (car-seqs seqs)
       (cond
         ((null? seqs) null)
         (else (cons (car (car seqs)) (car-seqs (cdr seqs))))))

(define (cdr-seqs seqs)
  (cond
    ((null? seqs) null)
    (else (cons (cdr (car seqs)) (cdr-seqs (cdr seqs))))))

(define test8
  (local
    ((define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))
    (begin
     (display "Test8\n")
     (display "seqs ") (display seqs) (newline)
     (display "car-seqs ") (display (car-seqs seqs)) (newline)
     (display "cdr-seqs ") (display (cdr-seqs seqs)) (newline)
     (newline))))

test8

(define (accumulate-n f init seq . seqs)
  (cond
    ((null? seqs) (accumulate2 f init seq))
    (else
     (local
       ((define (iterate init seqs)
          (cond
            ((null? init) null)
            (else
             (cons (accumulate2 f (car init) (car-seqs seqs))
                   (iterate (cdr init) (cdr-seqs seqs)))))))
       (iterate init (cons seq seqs))))))
       
(define test9
  (local
    ((define list1 (list 1 2 3))
     (define list2 (list 4 5 6))
     (define list3 (list 7 8 9))
     (define list4 (list 10 11 12))
     (define init (list 0 1 2)))
    (begin
     (display "Test9\n")
     (display "list ") (display list1) (newline)
     (display "initial ") (display (car init)) (newline)
     (display "accumulate-n + ")
     (display (accumulate-n
               +
               (car init)
               list1))
     (newline)
     (display "lists ")
     (display list1) (display " ")
     (display list2) (display " ")
     (display list3) (display " ")
     (display list4) (display " ")
     (newline)
     (display "initial ") (display init) (newline)
     (display "accumulate-n + ")
     (display (accumulate-n
               +
               init
               list1
               list2
               list3
               list4))
     (newline))))

test9



