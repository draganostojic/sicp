;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname fixed_point2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (fixed-point f x tolerrance max-count)
  (local
    ((define (err x)
       (abs (- x (f x))))
     (define (done? x)
       (< (err x) tolerrance))
     (define (damp x)
       (* 0.5 (+ x (f x))))
     (define (iterate count x)
       (begin
         #|(display count) (display ": ")
         (display "x ") (display x) (display " ")
         (display "f ") (display (f x)) (display " ")
         (display "damp ") (display (damp x)) (display " ")
         (display "err ") (display (err x)) (display " ")
         (newline)|#
         (cond
           ((done? x)
            (begin
              (display "iterations: ") (display count) (newline)
              x))
           ((> count max-count) "Exceeded max count!!!")
           (else
            (iterate (+ count 1) (damp x)))))))
    (iterate 0 x)))

(define max-count 100)

(define (sqrt-fixed x)
  (fixed-point (lambda (y) (/ (exact->inexact x) y)) 1.0 0.00001 max-count))

(display "sqrt(2)")
(newline)
(sqrt-fixed 2)
(newline)

(define (cube-root-fixed x)
  (fixed-point (lambda (y) (/ (exact->inexact x) (* y y))) 1.0 0.00001 max-count))

(display "cube-root(8)")
(newline)
(cube-root-fixed 8)
(newline)

(display "six x = x")
(newline)
(fixed-point sin 1.0 0.00001 10000)
(newline)

(display "cos x = x")
(newline)
(fixed-point cos 1.0 0.00001 10000)
(newline)

(display "sin x + cos x = x")
(newline)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0 0.00001 10000)
(newline)

