;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname continued_frac) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 1.37 Continued fractions

(define (cont-frac n d k)
  (local
    ((define (iterate i acc)
       (if (= i 0)
           acc
           (iterate (- i 1) (exact->inexact (/ (n i) (+ (d i) acc)))))))
    (iterate k 0)))

; Golden ratio
(define phy (/ (+ 1 (sqrt 5)) 2.0))


(define (approx-phy k)
  (/ 1.0 (cont-frac (lambda (i) 1) (lambda(i) 1) k)))

(define (find-k tolerrance)
  (local
    ((define (done? approx-phy-k)
       (< (abs (- phy approx-phy-k)) tolerrance))
     (define (iterate k)
       (local
         ((define approx-phy-k (approx-phy k)))
         (if (done? approx-phy-k) k (iterate (+ k 1))))))
    (iterate 1)))

(define tolerrance 0.00001)
(display "tolerance ") (display tolerrance) (newline)
(define k (find-k tolerrance))
(display "required k terms ") (display k) (newline)
(define approx-phy-k (approx-phy k))
(display "approx phy ") (display approx-phy-k) (newline)
(display "exact  phy ") (display phy) (newline)

; Exercise 1.38 Expansion for e

(define (approx-e k)
  (local
    ((define (n i) 1)
     (define (d i)
       (cond
         ((= i 1) 1)
         ((= (remainder (- i 2) 3) 0) (+ (* (quotient (- i 2) 3) 2) 2))
         (else 1))))
    (+ 2 (cont-frac n d k))))

(display e) (newline)
(display (approx-e 100)) (newline)

; Exercise 1.39 tan approximation

(define (approx-tan x k)
  (local
    ((define (n i)
       (if (= i 1) x (* -1 (* x x))))
     (define (d i) (+ (* (- i 1) 2) 1)))
    (cont-frac n d k)))

(define pi_8 (/ pi 8))
(display (tan pi_8)) (newline)
(display (approx-tan pi_8 100)) (newline)

