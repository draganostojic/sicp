#lang racket

; Exercise 2.29

#|
; mobile consist of left and right rod
(define (make-mobile left right)
  (list left right)) ; (cons left (cons rigth null))

; rod has length
; on rod there is a structure susoended
; structure is either weight or another mobile
(define (make-branch length structure)
  (list length structure)) ; (cons length (cons structure null))

; selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
    (car (cdr branch)))
|#

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define test1
  (begin
   (display "Test1\n")
   (local
     ((define mobile
        (make-mobile
         (make-branch 4 (make-mobile
                         (make-branch 4 3)
                         (make-branch 3 4)))
         (make-branch 2 (make-mobile
                         (make-branch 4 10)
                         (make-branch 10 4))))))
     (display "mobile ") (display mobile) (newline)
     (display "car mobile ") (display (car mobile)) (newline)
     (display "car cdr mobile ") (display (car (cdr mobile))) (newline)
     (display "left ") (display (left-branch mobile)) (newline)
     (display "right ") (display (right-branch mobile)) (newline)
     (display "llen ") (display (branch-length (left-branch mobile))) (newline)
     (display "lstruct ") (display (branch-structure (left-branch mobile))) (newline)
     (local
       ((define lmob (branch-structure (left-branch mobile))))
       (begin
         (display "lmob ") (display lmob) (newline)
         (display "left ") (display (left-branch lmob)) (newline)
         (display "right ") (display (right-branch lmob)) (newline)
         (display "llen ") (display (branch-length (left-branch lmob))) (newline)
         (display "llstruct ") (display (branch-structure (left-branch lmob))) (newline)
         (display "llstruct-pair? ") (display (pair? (branch-structure (left-branch lmob)))) (newline)
         (newline)))
     (newline))))
  

test1

(define (total-weight mobile)
  (local
    ((define (branch-weight branch)
       (local
         ((define bstruct (branch-structure branch)))
       (cond
         ((pair? bstruct) (total-weight bstruct))
         (else bstruct)))))
    (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))))

(define test2
  (begin
   (display "Test2\n")
   (local
     ((define mobile
        (make-mobile
         (make-branch 4 (make-mobile
                         (make-branch 4 3)
                         (make-branch 3 4)))
         (make-branch 2 (make-mobile
                         (make-branch 4 10)
                         (make-branch 10 4))))))
     (display "mobile ") (display mobile) (newline)
     (display "total-weight ") (display (total-weight mobile)) (newline)
     (newline))))

test2

; NOTE: (pair? struct) returns true for struct that is 3. It appears that
; struct that is evaluated is not 3 but some other expression struct???

(define (balanced? mobile)
  (local
    ((define lb (left-branch mobile))
     (define rb (right-branch mobile))
     (define lblen (branch-length lb))
     (define rblen (branch-length rb))
     (define lbstruct (branch-structure lb))
     (define rbstruct (branch-structure rb))
     (define (torque len bstruct)
       (local
         ((define mobile? (pair? bstruct)))
         (cond
           ((pair? bstruct) (* len (total-weight bstruct)))
           (else (* len bstruct)))))
     (define ltorq (torque lblen lbstruct))
     (define rtorq (torque rblen rbstruct)))
    (and (= ltorq rtorq)
         (if (pair? lbstruct) (balanced? lbstruct) true)
         (if (pair? rbstruct) (balanced? rbstruct) true))))

(define test3
  (begin
   (display "Test3\n")
   (local
     ((define mobile3
        (make-mobile
         (make-branch 4 (make-mobile
                         (make-branch 4 3)
                         (make-branch 3 4)))
         (make-branch 2 (make-mobile
                         (make-branch 4 10)
                         (make-branch 10 4))))))
     (display "mobile3 ") (display mobile3) (newline)
     (display "balanced? ") (display (balanced? mobile3)) (newline))))

test3

