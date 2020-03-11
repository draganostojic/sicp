;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname is-prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;#lang racket
;(require test-engine/racket-tests)

; 1.2.6 Example: Testing for Primality

; Check if d | x, for d = 2 .. sqrtt(x)
(define (is-prime? x)
  (local
    ((define (is-divisible-by? d)
       (cond
         ((> (* d d) x) false)
         ((= (remainder x d) 0) true)
         (else (is-divisible-by? (+ d 1))))))
    (if (is-divisible-by? 2) false true)))

(check-expect (is-prime? 13) true)
(check-expect (is-prime? 22) false)
(check-expect (is-prime? 131071) true)
(check-expect (is-prime? 6700417) true)
(check-expect (is-prime? 2147483647) true)
(check-expect (is-prime? 2147483646) false)

; Statistical test: Checks for rand chosen 1 < a < x that a^x = a (mod x) (Fermat's little theorem)

; Use fast exponentiation algorithm
; x = bk * 2^k + ... + b0 * 2^0 (k+1 bits for x)
; a^x = a^(bk * 2^k + ... + b0 * 2^0) = (a^bk)^(2^k) * ...  = ((...(1 or a)^2)^2)...)^2 (k times) * ...
; Algorithm for exponentiating a:
; Initial: y <- 1, d <- a
; (if (= (remainder x 2) 1) (y<-y * d) nothing)
; d <- d^2, x <- x / 2
; Terminate when (= x 0)
; Result: d

(define (exp2 a x)
  (local
    ((define (iterate y d x)
       (cond
         ((= x 0) y)
         (else
          (let
           ((d-sqrd (* d d))
            (x-half (quotient x 2)))
            (if (= (remainder x 2) 1)
                (iterate (* y d) d-sqrd x-half)
                (iterate y d-sqrd x-half)))))))
    (iterate 1 a x)))

(check-expect (exp2 2 3) 8)
(check-expect (exp2 2 7) 128)
(check-expect (exp2 2 32) 4294967296)
(check-expect (exp2 5 11) 48828125)

; Statistical test for primality

(define (is-prime2? x)
  (local
    ((define (iterate n)
       (let*
           ((a (random 1 x))
            (satisfies? (= (remainder (exp2 a x) x) a)))
         (cond
           ((not satisfies?) false)
           ((= n 1) satisfies?)
           (else (iterate (- n 1)))))))
    (iterate 2)))

(check-expect (is-prime2? 13) true)
(check-expect (is-prime2? 22) false)
(check-expect (is-prime2? 131071) true)
