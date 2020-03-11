;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 1.15

(define (sine x)
  (cond
    ((> x 0.01) (let ((p (sine (/ x 3.0)))) (- (* 3 p) (* 4 p p p))))
    (else x)))

(define test1 (sine 12.15))
test1
(abs (- test1 (sin 12.15)))

(define test2 (sine (/ pi 2.0)))
test2
(abs (- test2 (sin (/ pi 2.0))))

