#lang racket

(define run-tests? true)

; Exercise 2.37

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (test1 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define v (list 1 2 3))
        (define w (list 4 5 6))
        (define res (dot-product v w)))
       (begin
         (display "Test1\n")
         (display "v ") (display v) (newline)
         (display "w ") (display w) (newline)
         (display "dot-product ") (display res) (newline)
         (newline))))))

(test1 run-tests?)

(define (matrix-*-vector m v)
  (map (λ (r) (dot-product r v)) m))

(define (test2 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define v (list 1 2 3 4))
        (define m
          (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
        (define res (matrix-*-vector m v)))
       (begin
         (display "Test2\n")
         (display "v ") (display v) (newline)
         (display "m ") (display m) (newline)
         (display "matrix-*-vector ") (display res) (newline)
         (newline))))))

(test2 run-tests?)

(define (transpose m)
  (apply map list m))


(define (test3 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define m
          (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
        (define res (transpose m)))
       (begin
         (display "Test3\n")
         (display "m ") (display m) (newline)
         (display "transpose ") (display res) (newline)
         (newline))))))

(test3 run-tests?)

(define (matrix-*-matrix m n)
  (transpose (map (λ (v) (matrix-*-vector m v)) (transpose n))))

;one less transpose
(define (matrix-*-matrix2 m n)
  (map (λ (v) (matrix-*-vector (transpose n) v)) m))


(define (test4 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define m
          (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
        (define n
          (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)
                (list 6 7 8 9)))
        (define res (matrix-*-matrix m n))
        (define res2 (matrix-*-matrix2 m n)))
       (begin
         (display "Test4\n")
         (display "m ") (display m) (newline)
         (display "n ") (display n) (newline)
         (display "matrix-*-matrix  ") (display res) (newline)
         (display "matrix-*-matrix2 ") (display res2) (newline)
         (newline))))))

(test4 run-tests?)

(define (interval min max step)
  (cond
    ((> min max) null)
    (else (cons min (interval (+ min step) max step)))))

(define (test5 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define min 0)
        (define max 100)
        (define step 10)
        (define res (interval min max step)))
       (begin
         (display "Test5\n")
         (display "min ") (display min) (newline)
         (display "max ") (display max) (newline)
         (display "step ") (display step) (newline)
         (display "interval  ") (display res) (newline)
         (newline))))))

(test5 run-tests?)

(define (test6 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define min -5)
        (define max 5)
        (define step 0.5)
        (define res (interval min max step)))
       (begin
         (display "Test6\n")
         (display "min ") (display min) (newline)
         (display "max ") (display max) (newline)
         (display "step ") (display step) (newline)
         (display "interval  ") (display res) (newline)
         (newline))))))

(test6 run-tests?)

; Exercise 2.38

(define (fold-l op initial seq)
  (local
    ((define (iterate seq acc)
       (cond
         ((null? seq) acc)
         (else (iterate (cdr seq) (op (car seq) acc))))))
    (cond
      ((null? seq) initial)
      (else (iterate (cdr seq) (op (car seq) initial))))))

(define (test7 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 4))
        (define op cons)
        (define initial null)
        (define res (fold-l op initial seq)))
       (begin
         (display "Test7\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-l ") (display res) (newline)
         (newline))))))

(test7 run-tests?)

(define (test9 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq null)
        (define op cons)
        (define initial 3)
        (define res (fold-l op initial seq)))
       (begin
         (display "Test9\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-l ") (display res) (newline)
         (newline))))))

(test9 run-tests?)

(define (fold-r op initial seq)
  (cond
    ((null? seq) initial)
    (else
     (local
       ((define (iterate seq)
          (cond
            ((null? seq) initial)
            (else (op (car seq) (iterate (cdr seq) ))))))
       (iterate seq)))))

(define (test10 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 4))
        (define op cons)
        (define initial null)
        (define res (fold-r op initial seq)))
       (begin
         (display "Test10\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-l ") (display res) (newline)
         (newline))))))

(test10 run-tests?)

(define (test11 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq null)
        (define op cons)
        (define initial 3)
        (define res (fold-r op initial seq)))
       (begin
         (display "Test11\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-r ") (display res) (newline)
         (newline))))))

(test11 run-tests?)

(define (test12 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 ))
        (define op /)
        (define initial 1)
        (define res (fold-r op initial seq)))
       (begin
         (display "Test12\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-r ") (display res) (newline)
         (newline))))))

(test12 run-tests?) 

(define (test8 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 ))
        (define op /)
        (define initial 1)
        (define res (fold-l op initial seq)))
       (begin
         (display "Test8\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-l ") (display res) (newline)
         (newline))))))

(test8 run-tests?)  


(define (test13 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 ))
        (define op list)
        (define initial null)
        (define res (fold-r op initial seq)))
       (begin
         (display "Test13\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-r ") (display res) (newline)
         (newline))))))

(test13 run-tests?) 

(define (test14 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 ))
        (define op list)
        (define initial null)
        (define res (fold-l op initial seq)))
       (begin
         (display "Test14\n")
         (display "seq ") (display seq) (newline)
         (display "op ") (display op) (newline)
         (display "initial ") (display initial) (newline)
         (display "fold-l ") (display res) (newline)
         (newline))))))

(test14 run-tests?)


; Exercise 2.39

(define (reverse-from-foldr seq)
  (fold-r (λ (x acc) (append acc (list x))) null seq))

(define (test15 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 4))
        (define res (reverse-from-foldr seq)))
       (begin
         (display "Test15\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldr ") (display res) (newline)
         (newline))))))

(test15 run-tests?)

(define (test16 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq null)
        (define res (reverse-from-foldr seq)))
       (begin
         (display "Test16\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldr ") (display res) (newline)
         (newline))))))

(test16 run-tests?)

(define (test17 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1))
        (define res (reverse-from-foldr seq)))
       (begin
         (display "Test17\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldr ") (display res) (newline)
         (newline))))))

(test17 run-tests?)

(define (reverse-from-foldl seq)
  (fold-l cons null seq))

(define (test18 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1 2 3 4))
        (define res (reverse-from-foldl seq)))
       (begin
         (display "Test18\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldl ") (display res) (newline)
         (newline))))))

(test18 run-tests?)

(define (test19 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq null)
        (define res (reverse-from-foldl seq)))
       (begin
         (display "Test19\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldl ") (display res) (newline)
         (newline))))))

(test19 run-tests?)

(define (test20 run?)
  (cond
    ((not run?) (void))
    (else
     (local
       ((define seq (list 1))
        (define res (reverse-from-foldl seq)))
       (begin
         (display "Test20\n")
         (display "seq ") (display seq) (newline)
         (display "reverse-from-foldl ") (display res) (newline)
         (newline))))))

(test20 run-tests?)
  