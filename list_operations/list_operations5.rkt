#lang racket

(define show? true)

(define problem-descr
#<<here-string-problem-descr

We wish to generate all the permutations of a set S;
that is, all the ways of ordering the items in the set.


here-string-problem-descr
)

(cond
  (show? (display problem-descr))
  (else (void)))

; perm(S) = U { s x perm(S\s) : s from S }
; perm(null) = null

(define (set-without2 element set)
  (cond
    ((null? set) null)
    ((equal? (car set) element) (cdr set))
    (else (cons (car set) (set-without2 element (cdr set))))))


(define (test1 show?)
  (cond
    (show?
     (local
       ((define list1 (list 1 2 3 4))
        (define el 1)
        (define res1 (set-without2 el list1))
        (define list2 (list 1 2))
        (define res2 (set-without2 el list2))
        (define list3 (list 1))
        (define res3 (set-without2 el list3)))
       (begin
        (display "Test1") (newline)
        (display "element ") (display el) (display " list ") (display list1) (newline)
        (display "set-without2 ") (display res1) (newline)
        (display "element ") (display el) (display " list ") (display list2) (newline)
        (display "set-without2 ") (display res2) (newline)
        (display "element ") (display el) (display " list ") (display list3) (newline)
        (display "set-without2 ") (display res3) (newline)
       (newline))))
    (else (void))))

;(test1 show?)

; use filter
(define (set-without el set)
  (filter (λ (e) (not (equal? e el))) set))


(define (test2 show?)
  (cond
    (show?
     (local
       ((define list1 (list 1 2 3 4))
        (define el 1)
        (define res1 (set-without el list1))
        (define list2 (list 1 2))
        (define res2 (set-without el list2))
        (define list3 (list 1))
        (define res3 (set-without el list3)))
       (begin
        (display "Test2") (newline)
        (display "element ") (display el) (display " list ") (display list1) (newline)
        (display "set-without ") (display res1) (newline)
        (display "element ") (display el) (display " list ") (display list2) (newline)
        (display "set-without ") (display res2) (newline)
        (display "element ") (display el) (display " list ") (display list3) (newline)
        (display "set-without ") (display res3) (newline)
       (newline))))
    (else (void))))

(test2 show?)

(define (combine x seqs)
  (cond
    ((or (null? seqs) (not (pair? (car seqs)))) (list (cons x seqs)))
    (else
     (map (λ (seq)
            (cons x seq))
          seqs))))

(define (test3 show?)
  (cond
    (show?
     (local
       ((define list1 (list
            (list 2 3 4)
            (list 2 3)
            (list 2 4)
            (list 3 4)
            null))
        (define el1 1)
        (define res1 (combine el1 list1))
        (define list2 (list 3 4))
        (define el2 2)
        (define res2 (combine el2 list2))
        (define list3 null)
        (define el3 3)
        (define res3 (combine el3 list3)))
       (begin
        (display "Test3") (newline)
        (display "element ") (display el1) (display " list ") (display list1) (newline)
        (display "combine ") (display res1) (newline)
        (display "element ") (display el2) (display " list ") (display list2) (newline)
        (display "combine ") (display res2) (newline)
        (display "element ") (display el3) (display " list ") (display list3) (newline)
        (display "combine ") (display res3) (newline)
       (newline))))
    (else (void))))

(test3 show?)

(define (perms set)
  (foldr append null (map (λ (e) (combine e (perms (set-without e set)))) set)))

(define (test4 show?)
  (cond
    (show?
     (local
       ((define list1 (list 1 2))
        (define res1 (perms list1))
        (define list2 (list 1 2 3))
        (define res2 (perms list2))
        (define list3 (list 1 2 3 4))
        (define res3 (perms list3))
        (define list4 (list 'a 'b 'c 'd 'e 'f))
        (define res4 (perms list4))
        (define (show-list list)
          (foldl (λ (el acc)
                   (begin
                     (display acc) (display " ") (display el) (newline)
                     (+ acc 1)))
                 1
                 list)))
       (begin
        (display "Test4") (newline)
        (display "list ") (display list1) (newline)
        (display "permutations") (newline) (show-list res1) (newline)
        (display "list ") (display list2) (newline)
        (display "permutations") (newline) (show-list res2) (newline)
        (display "list ") (display list3) (newline)
        (display "permutations") (newline) (show-list res3) (newline)
        (display "list ") (display list4) (newline)
        (display "permutations") (newline) (show-list res4) (newline)
       (newline))))
    (else (void))))

(test4 show?)





  
