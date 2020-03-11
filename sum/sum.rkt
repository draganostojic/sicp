;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; 1.3.1 Procedures as Arguments
; Various sum abstractions

; sum of integers

; recursive
(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (+ 1 a) b))))

(check-expect (sum-int 1 10) 55)

; iterative
(define (sum-int2 a b)
  (local
    ((define (iterate acc x)
       (if (> x b)
           acc
           (iterate (+ acc x) (+ 1 x)))))
    (iterate 0 a)))

(check-expect (sum-int2 1 10) 55)

; sum of cubes

; recursive
(define (cube x)
  (* x x x))

(define (sum-cube a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cube (+ 1 a) b))))

(check-expect (sum-cube 1 10) 3025)

; summation (iterative)

(define (sum-abstract a b f next)
  (local
    ((define (iterate acc x)
       (if (> x b)
           acc
           (iterate (+ acc (f x)) (next x)))))
    (iterate 0 a)))


; sum of integers
(define (sum-int3 a b)
  (local
    ((define (f x) x)
     (define (next x) (+ x 1)))
    (sum-abstract a b f next)))

(check-expect (sum-int3 1 10) (sum-int2 1 10))


; sum of cubes of integers
(define (sum-cube2 a b)
  (local
    ((define (f x) (* x x x))
     (define (next x) (+ x 1)))
    (sum-abstract a b f next)))

(check-expect (sum-cube2 1 10) (sum-cube 1 10))

; sum-pi
; 1/(1*3) + 1/(5*7) + ... = pi / 4

(define (sum-pi a b)
  (local
    ((define (f x) (/ 1.0 (* x (+ x 2))))
     (define (next x) (+ x 4)))
    (sum-abstract a b f next)))

(define pi2 (* 8.0 (sum-pi 1 1000)))
pi2
(abs (- pi2 pi))

; integral
; f(a + dx/2) + f(a + dx/2 + dx) + f(a + dx/2 + 2dx) ...

(define (integral a b f dx)
  (local
    ((define (next x) (+ x dx)))
    (* (sum-abstract (+ a (/ dx 2.0)) b f next) dx)))

(integral 0 1 cube 0.01)
(integral 0 1 cube 0.001)

; simpson's integration rule
; h/3 * (y0 + 4y1 + 2y2 + ... + 4yn-1 + yn)
; h = (b - a)/n, yk=f(a + kh) and n is even

(define (integral-simpson a b f n)
  (local
    ((define h (* (/ 1.0 n) (- b a)))
     (define (next k) (+ k 1))
     (define (g k)
       (let
         ((y (f (+ a (* k h)))))
         (cond
           ((or (= k 0) (= k n)) y)
           ((= (remainder k 2) 1) (* 4 y))
           (else (* 2 y))))))
    (* (/ h 3.0) (sum-abstract 0 n g next))))

(integral-simpson 0 1 cube 100)
(integral-simpson 0 1 cube 1000)
(integral-simpson 0 pi sin 1000)

; Abstract product
; Exercise 1.31

(define (product a b f next)
  (local
      ((define (iterate k acc)
         (cond
           ((> k b) acc)
           (else (iterate (next k) (* acc (f k)))))))
    (iterate a 1)))

(define (fact2 n)
  (local
    ((define (f k) k)
     (define (next k) (+ k 1)))
    (product 1 n f next)))

(require math/number-theory)
(check-expect (fact2 10) (factorial 10))

; John Willis formula: pi/4=2/3*4/3*4/5*6/5...

(define (pi-john-willis n)
  (local
    ((define (f k)
       (local
         ((define k1 (* k 2))
          (define k2 (+ k1 2))
          (define k3 (+ k1 1)))
         (/ (* k1 k2) (* k3 k3))))
     (define (next k) (+ k 1)))
    (* (product 1 n f next) 4)))

(define pi3 (pi-john-willis 10000))
pi3
(abs (- pi3 pi))

; abstract accumulate
; null value is inital accumulator

(define (accumulate combiner null-value f a next b)
  (local
    ((define (iterate k acc)
       (cond
         ((> k b) acc)
         (else (iterate (next k) (combiner acc (f k)))))))
    (iterate a null-value)))

; sum implemented by accumulator
(define (sum-accumulate f a next b)
  (accumulate + 0 f a next b))

; simpson's integration rule implemented by sum-accumulate
; h/3 * (y0 + 4y1 + 2y2 + ... + 4yn-1 + yn)
; h = (b - a)/n, yk=f(a + kh) and n is even

(define (integral-simpson2 a b f n)
  (local
    ((define h (* (/ 1.0 n) (- b a)))
     (define (next k) (+ k 1))
     (define (g k)
       (let
         ((y (f (+ a (* k h)))))
         (cond
           ((or (= k 0) (= k n)) y)
           ((= (remainder k 2) 1) (* 4 y))
           (else (* 2 y))))))
    (* (/ h 3.0) (sum-accumulate g 0 next n))))

(abs (- (integral-simpson 0 1 cube 100) (integral-simpson2 0 1 cube 100)))
(abs (- (integral-simpson 0 1 cube 1000) (integral-simpson2 0 1 cube 1000)))
(abs (- (integral-simpson 0 pi sin 1000) (integral-simpson2 0 pi sin 1000)))

; product implemented by accumulator
(define (product-accumulate f a next b)
  (accumulate * 1 f a next b))


; John Willis formula by product-accumulate: pi/4=2/3*4/3*4/5*6/5...

(define (pi-john-willis2 n)
  (local
    ((define (f k)
       (local
         ((define k1 (* k 2))
          (define k2 (+ k1 2))
          (define k3 (+ k1 1)))
         (/ (* k1 k2) (* k3 k3))))
     (define (next k) (+ k 1)))
    (* (product-accumulate f 1 next n) 4)))

(define pi4 (pi-john-willis2 10000))
pi4
(abs (- pi4 pi))

; accumulate with the filter
; filter selects which terms are to be combined
; null-value is initial accumulator

(define (accumulate2 filter combiner null-value f a next b)
  (local
    ((define (iterate k acc)
       (cond
         ((> k b) acc)
         (else (iterate (next k) (if (filter k) (combiner (f k) acc) acc))))))
    (iterate a null-value)))

; product implemented by accumulate with filter
(define (product-accumulate2 filter f a next b)
  (accumulate2 filter * 1 f a next b))

; John Willis formula by product-accumulate2: pi/4=2/3*4/3*4/5*6/5...

(define (pi-john-willis3 n)
  (local
    ((define (f k)
       (local
         ((define k1 (* k 2))
          (define k2 (+ k1 2))
          (define k3 (+ k1 1)))
         (/ (* k1 k2) (* k3 k3))))
     (define (next k) (+ k 1))
     (define (filter k) true))
    (* (product-accumulate2 filter f 1 next n) 4)))

(define pi5 (pi-john-willis3 10000))
pi5
(abs (- pi5 pi))

; the product of all the positive integers less than n that are relatively prime
; to n (i.e., all positive integersi <nsuchthatGCD(i,n)=1).

(define (prod-relprime n)
  (local
    ((define (f k) k)
     (define (gcd x y)
       (cond
         ((= y 0) x)
         ((< x y) (gcd x (remainder y x)))
         (else (gcd y (remainder x y)))))
     (define (filter k) (= (gcd k n) 1))
     (define (next k) (+ k 1)))
    (product-accumulate2 filter f 1 next n)))


(check-expect (prod-relprime 10) (* 1 3 7 9))




                  
