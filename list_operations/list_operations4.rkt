#lang racket

; Nested Mappings

(define problem-descr
#<<here-string-problem-descr
Given a positive integer n find all ordered pairs of distinct
positive integers i and j , where 1 ≤ j < i ≤ n, such that i + j is prime
here-string-problem-descr
  )

(require math/number-theory)

(define (seq n)
  (local
    ((define (enum n)
       (local
         ((define (iterate i)
            (cond
              ((> i n) null)
              (else (cons i (iterate (+ i 1)))))))
         (iterate 1)))
;     (define enum-i (cdr (enum n)))
;     (define enum-j
;       (map (λ (i)
;              (cond
;                ((> i 0) (enum (- i 1)))
;                (else null))) enum-i))
;     (define enum-i-j2
;       (foldr (λ (i seq-j acc)
;                (cons (cons i seq-j) acc))
;              null
;              enum-i
;              enum-j))
;     (define enum-i-j3
;       (foldr (λ (i seq-j acc)
;                (append (map (λ (j)
;                       (list i j (+ i j))) seq-j)
;                        acc))
;              null
;              enum-i
;              enum-j))
     (define enum-i-j
       (foldr append
              null
              (map
               (λ (i)
                 (map
                  (λ (j)
                    (list i j (+ i j)))
                  (enum (- i 1))))
               (cdr (enum n)))))
;     (define (flat-map proc seq)
;       (foldr append
;              null
;              (map proc seq)))
;     (define enum-i-j4
;       (flat-map (λ (i)
;                   (map
;                    (λ (j)
;                      (list i j (+ i j)))
;                    (enum (- i 1))))
;                 (cdr (enum n))))
     (define i-j-prime-sum-seq
       (filter (λ (i-j-sum)
                 (prime? (car (cdr (cdr i-j-sum)))))
               enum-i-j)))
     (begin
;       (display "i ") (display enum-i) (newline)
;       (display "j ") (display enum-j) (newline)
;       (display "i-j2 ") (display enum-i-j2) (newline)
;       (display "i-j3 ") (display enum-i-j3) (newline)
;       (display "i-j4 ") (display enum-i-j4) (newline)
;       (display "i-j  ") (display enum-i-j) (newline)
;       (display "i-j-prime-sum-seq ") (display i-j-prime-sum-seq) (newline)
;       (newline)
       i-j-prime-sum-seq)))

;(foldr cons null (list 1 2 3 4))

;(seq 6)

(display problem-descr)
(newline)
(newline)

(define show? true)

(define (test1 show?)
  (cond
    ((not show?) (void))
    (else
     (local
       ((define n 6)
        (define res (seq n)))
       (begin
         (display "Test1\n")
         (display "n ") (display n) (newline)
         (display "result ") (display res) (newline))))))

(test1 show?)





