#lang racket

(define (scale-list l s)
  (map (λ (x) (* x s)) l))

(define test-scale-list
  (local
    ((define l (list 1 2 3)))
    (begin
     (display "test-scale-list\n")
     (display l) (newline)
     (display (scale-list l 3)))))

test-scale-list

(define (scale-tree t s)
  (map (λ (x)  ; map replaces x with λ expression
         (cond
           ((pair? x) (scale-tree x s))
           (else (* x s)))) t))

(define test-scale-tree1
  (local
    ((define tree1
       (cons (list 1 2) (list 3 4)))
     (define scale1 3)
     (define big-tree
       (list (list (list 1 2 3 19 283 38) 2 3 2) (list 2 3 (list 217 382 1827) 2 187 (list 2838))
        2 1 2 (list 2 (list 3 (list 3)) 23 2 1 238)))
     (define scale2 2))
    (begin
     (display "test-scale-tree1\n")
     (display "tree: ") (display tree1) (newline)
     (display "scaled by ") (display scale1) (display " ") (display (scale-tree tree1 scale1)) (newline)
     (display "tree: ") (display big-tree) (newline)
     (display "scaled by ") (display scale2) (display " ") (display (scale-tree big-tree scale2)) (newline))))

test-scale-tree1






