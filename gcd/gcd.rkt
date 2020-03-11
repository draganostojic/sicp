;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname gcd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; 1.2.5 Greatest Common Divisors

(define (gcd2 x y)
  (cond
    ((= y 0) x)
    (else (if (< x y) (gcd2 x (remainder y x)) (gcd2 y (remainder x y))))))

(check-expect (gcd2 206 40) 2)
(check-expect (gcd2 40 206) 2)
