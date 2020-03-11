#lang racket

; list representation of trees


; tree node is a list

(define (tree-node? x)
  (list? x))

; tree leafs are structures in the node
(define (tree-leaves struct? node)
  (cond
    ((not (tree-node? node)) false)
    (else    
     (local
       ((define (recurse node)
          (cond
           ((null? node) '())
           ((struct? (car node)) (cons (car node) (recurse (cdr node))))
           (else (recurse (cdr node))))))
       (recurse node)))))

; tree branches are list in the node
(define (tree-branches node)
  (cond
    ((not (tree-node? node)) false)
    (else
     (local
       ((define (recurse node)
          (cond
            ((null? node) '())
            ((list? (car node)) (cons (car node) (recurse (cdr node))))
            (else (recurse (cdr node))))))
       (recurse node)))))

(display "tree of numbers\n")

(define tree-of-numbers
  (cons (list 1 2) (list 3 4)))

(tree-node? tree-of-numbers)

(tree-leaves number? tree-of-numbers)

(tree-branches tree-of-numbers)

(display "tree of numbers2\n")

(define tree-of-numbers2
  (list tree-of-numbers tree-of-numbers))

(tree-node? tree-of-numbers2)

(tree-leaves number? tree-of-numbers2)

(tree-branches tree-of-numbers2)

(define (count-leaves struct? tree)
  (local
    ((define (recurse node)
       (cond
         ((struct? node) 1)
         ((not (list? node)) false)
         ((null? node) 0)
         (else
          (local
            ((define result-cdr (recurse (cdr node)))
             (define result-car (recurse (car node))))
            (cond
              ((or (false? result-cdr) (false? result-car)) false)
              (else (+ result-cdr result-car))))))))
    (recurse tree)))

(display "count leaves in tree of numbers\n")
(count-leaves number? tree-of-numbers)

(display "count leaves in tree of numbers2\n")
(count-leaves number? tree-of-numbers2)

; Exercise 2.28
(define (fringe tree leaf?)
  (local
    ((define (recurse node)
       (cond
         ((null? node) '())
         ((leaf? node) (cons node '()))
         ((pair? node) (append (recurse (car node)) (recurse (cdr node))))
         (else false))))
    (recurse tree)))

(display "fringe\n")

(define x (list (list 1 2) (list 3 4)))
(fringe x number?)

(fringe (list x x) number?)


(define (fringe-dump tree leaf?)
  (local
    ((define (iterate-node node branches)
       (cond
         ((null? node)
          (begin
            (newline)
            (iterate-branch branches)))
         ((leaf? (car node))
          (begin
            (display (car node))
            (newline)
            (iterate-node (cdr node) branches)))
         (else (iterate-node (cdr node) (cons (car node) branches)))))
    (define (iterate-branch branches)
      (cond
        ((null? branches) (void))
        (else (iterate-node (car branches) (cdr branches))))))
  (iterate-node tree '())))


(display "fringe-dump\n")

(fringe-dump (list x x) number?)

(display "fringe-dump 2\n")

(fringe-dump (list (list 1 2) (list 3 4)) number?)

            