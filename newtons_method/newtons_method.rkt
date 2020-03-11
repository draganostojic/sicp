;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname newtons_method) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Newtown's method for solving f(x)
; It's finding fixed point of g(x) = x - f(x)/f'(x)

; Fixed point (bare bones)

(define (fixed-point f guess tollerance)
  (local
    ((define x0 (exact->inexact guess))
     (define e (exact->inexact tollerance))
     (define (done? x)
       (< (abs (- (f x) x)) e))
     (define (iterate i x)
       (begin
         (display i) (display " ") (display x) (newline)
         (cond
           ((done? x) x)
           (else (iterate (+ i 1) (f x)))))))
    (iterate 0 x0)))

(define (newtons-method f guess tollerance)
  (local
    ((define dx tollerance)
     (define (derivative f)
       (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))))
    (fixed-point (lambda (x) (- x (/ (f x) ((derivative f) x)))) guess tollerance)))

(define (sqrt-newton x tollerance)
  (newtons-method (lambda (y) (- (* y y) x)) 1.0 tollerance))

(display "sqrt(2)") (newline)
(display (sqrt-newton 2 0.00001))
(newline)

(display "sqrt(16)") (newline)
(display (sqrt-newton 16 0.00001))
(newline)

(display "cube-root(8)") (newline)
(display (newtons-method (lambda (y) (- (* y y y) 8)) 1.0 0.00001))
(newline)



