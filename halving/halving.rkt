;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname halving) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Finding roots of equations by the half- interval method

(define (root-half f a b error)
  (local
    ((define x (if (< (f a) 0) a b))
     (define y (if (> (f b) 0) b a))
     (define (iterate x y)
       (local
         ((define half (/ (+ x y) 2.0))
          (define mid (f half)))
         (cond
           ((= mid 0) half)
           ((< (abs mid) error) half)
           ((< mid 0) (iterate half y))
           (else (iterate x half))))))
    (cond
      ((and (> (f a) 0) (> (f b) 0)) false)
      ((and (< (f a) 0) (< (f b) 0)) false)
      ((= a 0) a)
      ((= b 0) b)
      (else (iterate x y)))))

(define pi1 (root-half sin 2.0 4.0 0.0000001))
pi1
(abs (- pi1 pi))

(check-expect (root-half sin 2.0 3.0 0.1) false)
(check-expect (root-half sin 4.0 5.0 0.1) false)
(check-expect (root-half (lambda (x) x) 0 10 0.1) 0)
(check-expect (root-half (lambda (x) x) -3 0 0.1) 0)
(check-expect (root-half (lambda (x) x) -3 -3 0.1) false)
(check-expect (root-half (lambda (x) x) 2 2 0.1) false)

