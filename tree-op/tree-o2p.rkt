#lang racket


; Exercise 2.18
(display "reverse\n")

(define (reverse l)
  (local
    ((define (iterate res l)
       (cond
         ((null? l) res)
         (else (iterate (cons (car l) res) (cdr l))))))
    (iterate '() l)))

(list 1 4 9 16 25)
(reverse (list 1 4 9 16 25))

; Exercise 2.27
(display "deep-reverse\n")

(define (deep-reverse l)
  (local
    ((define (iterate res l)
       (cond
         ((null? l) res)
         (else (iterate (cons
                         (if (pair? (car l))
                             (deep-reverse (car l))
                             (car l))
                         res) (cdr l))))))
    (iterate '() l)))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2 (list 3 4)) (list 5 (list 6 7 (list 8 9) 10) 11)))

x

(reverse x)

;; Unbelivable that this works!!!
(deep-reverse x)

y

(reverse y)

(deep-reverse y)

; Exercise 2.28
(display "fringe\n")

(define (fringe l)
  (local
    ((define (iterate res l)
       (cond
         ((null? l) res)
         ((pair? (car l)) (iterate (append res (fringe (car l))) (cdr l)))
         (else (iterate (append res (list (car l))) (cdr l))))))
    (iterate '() l)))

x
(fringe x)
(list x x)
(fringe (list x x))

(cons (list 1 2) (list 3 4))
(fringe (cons (list 1 2) (list 3 4)))


(define big-tree
  (list (list (list 1 2 3 19 283 38) 2 3 2) (list 2 3 (list 217 382 1827) 2 187 (list 2838))
                2 1 2 (list 2 (list 3 (list 3)) 23 2 1 238)))

big-tree
(fringe big-tree)


