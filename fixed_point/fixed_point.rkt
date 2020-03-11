;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname fixed_point) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Finding fixed points of function
; This is x where y = f(x) intersects y = x
; NOTE: begin function is used for sequencing

(define (fixed-point f x tolerrance)
  (local
    ((define (good-enough? x)
       (< (abs (- (f x) x)) tolerrance))
     (define (iterate x acc)
       (if (good-enough? x)
           (begin
             (display acc)
             (display " iterations")
             (newline)
             x)
           (begin
             ;(display acc) (display " x: ") (display x) (newline)
             (iterate (f x) (+ 1 acc))))))
    (iterate x 0)))

; average damp version
; approach of average damping successive approximations helps in fixed-point
; searches

(define (fixed-point2 f x tolerrance)
  (local
    ((define (damp x)
       (* 0.5 (+ x (f x)))))
    (fixed-point damp x tolerrance)))

(define (test-name name)
  (begin
  (display name)
  (newline)
  (display "=============")
  (newline)))

(test-name "cox x = x")
(fixed-point cos 1.0 0.00001)
(newline)

(test-name "average-damp")
(fixed-point2 cos 1.0 0.00001)
(newline)

(test-name "sin x = x")
(define x1 
  (fixed-point sin 1.0 0.00001))
(newline)

(display "check")
(newline)
(display "x=")
(display x1)
(newline)
(display "sin x=")
(sin x1)
(newline)

(test-name "average-damp")
(define x2
  (fixed-point2 sin 1.0 0.00001))
(newline)

(display "check")
(newline)
(display "x=")
(display x2)
(newline)
(display "sin x=")
(sin x2)
(newline)

(test-name "y = sin y + cos y")
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0 0.00001)
(newline)

(test-name "average-damp")
(fixed-point2 (lambda (y) (+ (sin y) (cos y))) 1.0 0.00001)
(newline)

; Find square root of x
; y = x/y doesn't converge
; NOTE: x is fixed and initial guess is for y
; y = 1/2*(y + x/y) which is average of y and x/y converges

(define (sqrt2 x)
  (fixed-point (lambda (y) (* 0.5 (+ y (/ x y)))) 1.0 0.00001))

(define (sqrt-damp x)
  (fixed-point2 (lambda (y) (/ x y)) 1.0 0.00001))

(test-name "sqrt(2)")
(sqrt2 2)
(newline)

(test-name "average-damp")
(sqrt-damp 2)
(newline)

(define (cube-root x)
  (fixed-point (lambda (y) (/ x (* y y))) 1.0 0.00001))

;(test-name "cube-root(8)")
;(cube-root 8)
;(newline)



