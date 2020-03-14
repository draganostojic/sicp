#lang racket

(define show? true)

((λ (show?)
   (if (not show?)
       (void)
       (local
         ((define descr
            #<<here-string
Sets as ordered binary trees
here-string
            ))
         (display descr)
         (newline))))
 show?)

; (list node left right)

(define (make-btree node left right)
  (list node left right))

(define (btree-node btree)
  (cond
    ((null? btree) null)
    (else (car btree))))

(define (btree-left btree)
  (cond
    ((null? btree) null)
    (else (cadr btree)))) ; (cadr  is (car (cdr 

(define (btree-right btree)
  (cond
    ((null? btree) null)
    (else (caddr btree)))) ; caddr is (car (cdr (cdr

; I can verify that the btree is correctly formed by traversing the tree "in-order"
; which means flattening it into ordered list

(define (display-btree btree)
  (local
    ((define (iterate set)
       (cond
         ((null? set) (void))
         (else
          (begin
            (iterate (btree-left set))
            (display (btree-node set)) (display " ")
            (iterate (btree-right set)))))))
    (begin
      (display "{ ") (iterate btree) (display "}"))))

(define (btree->list2 btree)
  (local
    ((define (recurse btree)
       (cond
         ((null? btree) null)
         (else
          (local
            ((define lbt (btree-left btree))
             (define node (btree-node btree))
             (define rbt (btree-right btree)))
            (begin
              ;(display "lbt:") (display-btree lbt) (display " ")
              ;(display "node:") (display node) (display " ")
              ;(display "rbt:") (display-btree rbt) (newline)
              (cond
                ((and (null? lbt) (null? rbt)) (list node))
                ((null? lbt) (cons node (recurse rbt)))
                ((null? rbt) (append (recurse lbt) (list node)))
                (else (append (recurse lbt) (cons node (recurse rbt))))))))))
     (define result (recurse btree)))
    (begin
      ;(display result) (newline)
      result)))

; S = { 1 3 5 7 9 11 }
;(define s
;  (make-btree 5
;              (make-btree 3
;                          (make-btree 1 null null)
;                          null)
;              (make-btree 9
;                          (make-btree 7 null null)
;                          (make-btree 11 null null))))

;(btree->list s)

; Note: instead of thinking about representing sets as lists, we can think
; of representing lists as sets and list insertion op as set adjoin op.
; If a set is represented as a tree then adjoining a set is btree insertion operation

; iterative process
(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((< x (btree-node set)) (element-of-set? x (btree-left set)))
    ((= x (btree-node set)) true)
    (else (element-of-set? x (btree-right set)))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s
              (make-btree 5
                          (make-btree 3
                                      (make-btree 1 null null)
                                      null)
                          (make-btree 9
                                      (make-btree 7 null null)
                                      (make-btree 11 null null)))))
           (begin
             (local
               ((define x 3)
                (define res (element-of-set? x s)))
               (begin
                 (display x) (display " is element of ") (display (btree->list2 s)) (display " ") (display res)
                 (newline)))
             (local
               ((define x 4)
                (define res (element-of-set? x s)))
               (begin
                 (display x) (display " is element of ") (display (btree->list2 s)) (display " ") (display res)
                 (newline)))))
         (newline)
         (newline))))
 "Test1" show?)

(define (adjoin-set x set)
  (cond
    ((null? set) (make-btree x null null))
    ((= x (btree-node set)) set)
    ((< x (btree-node set)) (make-btree (btree-node set)
                                        (adjoin-set x (btree-left set))
                                        (btree-right set)))
    (else (make-btree (btree-node set)
                      (btree-left set)
                      (adjoin-set x (btree-right set))))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s
              (make-btree 5
                          (make-btree 3
                                      (make-btree 1 null null)
                                      null)
                          (make-btree 9
                                      (make-btree 7 null null)
                                      (make-btree 11 null null)))))
           (begin
             (display "set ") (display (btree->list2 s)) (newline)
             (local
               ((define x 3)
                (define res (adjoin-set x s)))
               (begin
                 (display "add ") (display x) (newline)
                 (display "result ") (display (btree->list2 res)) (newline)))
             (local
               ((define x 4)
                (define res (adjoin-set x s)))
               (begin
                 (display "add ") (display x) (newline)
                 (display "result ") (display (btree->list2 res)) (newline)))))
         (newline)
         (newline))))
 "Test2" show?)

; representing an ordered list as a binary tree
; is more efficient than representing it as a linked list
; if the tree is balanced
; insertion operation takes O(log n) while if it's
; kept as a linked list its complexity is O(n)
; same complexity O(log n) vs O(n) is to check membership

; Binary tree to list v2 (without append)
(define (btree->list btree)
  (cond
    ((null? btree) null)
    (else
     (local
       ((define (recurse btree result) ; Note that btree is binary tree and result is a list!
          (local
            ((define lbt (btree-left btree))
             (define rbt (btree-right btree))
             (define node (btree-node btree)))
            (begin
              ;(display "bt:") (display-btree btree) (display " ")
              ;(display "l:") (display-btree lbt) (display " ")
              ;(display "e:") (display node) (display " ")
              ;(display "r:") (display-btree rbt) (display " ")                              
              ;(display "result:") (display result) (newline)
              (cond
                ((and (null? lbt) (null? rbt)) (cons node result))
                ((null? lbt) (cons node (recurse rbt result)))
                ((null? rbt) (recurse lbt (cons node result)))
                (else (recurse lbt (cons node (recurse rbt result))))))))
        (define result (recurse btree null)))
       (begin
         ;(display "result:") (display result) (newline)
         result)))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s
              (make-btree 5
                          (make-btree 3
                                      (make-btree 1 null null)
                                      null)
                          (make-btree 9
                                      (make-btree 7 null null)
                                      (make-btree 11 null null)))))
           (begin
             (display "set ") (display (btree->list s)) (newline)
             (local
               ((define x 3)
                (define res (adjoin-set x s)))
               (begin
                 (display "add ") (display x) (newline)
                 (display "result ") (display (btree->list res)) (newline)))
             (local
               ((define x 4)
                (define res (adjoin-set x s)))
               (begin
                 (display "add ") (display x) (newline)
                 (display "result ") (display (btree->list res)) (newline)))))
         (newline)
         (newline))))
 "Test3" show?)

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s
             (adjoin-set 1
             (adjoin-set 2
             (adjoin-set 3
             (adjoin-set 4
             (adjoin-set 5
             (adjoin-set 6
             (adjoin-set 7
             (adjoin-set 8
             (adjoin-set 9
             (adjoin-set 10
             (adjoin-set 11
             (adjoin-set 12
             (adjoin-set 13 null)))))))))))))))
           (begin
             (display "btree ") (display-btree s) (newline)
             (display "btree->list\n")
             (display (btree->list2 s)) (newline)
             (display "btree->list2\n")
             (display (btree->list s)) (newline)))
         (newline)
         (newline))))     
 "Test4" show?)

; create a balanced binary tree from an odered list
; idea is to adjoin elements starting from the middle
; element in the list and to then adjoin elements from
; the list on the left and from the list on the right
; and continue recursevly

; take n right most elements
(define (lright n l)
   (cdr
    (foldr
     (λ (x acc)
       (if (>= (car acc) n)
           acc
           (cons (+ (car acc) 1) (cons x (cdr acc)))))
     (cons 0 null)
     l)))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l (list 1 2 3 4 5 6 7))
            (define n 0)
            (define result (lright n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lright ") (display result) (newline)))
         (local
           ((define l (list 1 2 3 4 5 6 7))
            (define n 3)
            (define result (lright n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lright ") (display result) (newline)))
         (newline)
         (newline))))
 "Test5" show?)

; take n left most elements
(define (lleft n l)
     (local
     ((define (recurse i l)
        (cond
          ((null? l) null)
          ((>= i n) null)
          (else (cons (car l) (recurse (+ i 1) (cdr l)))))))
     (recurse 0 l)))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l (list 1 2 3 4 5 6 7))
            (define n 0)
            (define result (lleft n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lleft ") (display result) (newline)))
         (local
           ((define l (list 1 2 3 4 5 6 7))
            (define n 3)
            (define result (lleft n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lleft ") (display result) (newline)))
         (newline)
         (newline))))
 "Test6" show?)

(define (lsize l)
  (foldr (λ (x acc) (+ 1 acc)) 0 l))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l (list 1 2 3 4 5 6 7))
            (define result (lsize l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsize ") (display result) (newline)))
         (newline)
         (newline))))
 "Test7" show?)

(define (lselect n l)
  (local
    ((define (iterate i l)
       (cond
         ((= i n) (car l))
         (else (iterate (+ i 1) (cdr l))))))
    (iterate 1 l)))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l (list 'a 'b 'c 'd))
            (define n 1)
            (define result (lselect n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lselect ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd))
            (define n 2)
            (define result (lselect n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lselect ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd))
            (define n 3)
            (define result (lselect n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lselect ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd))
            (define n 4)
            (define result (lselect n l)))
           (begin
             (display "list ") (display l) (newline)
             (display "n ") (display n) (newline)
             (display "lselect ") (display result) (newline)))
         (newline)
         (newline))))
 "Test8" show?)

(define (lsplit l)
  (cond
    ((null? l) (list null null null))
    (else
     (local
       ((define n (lsize l))
        (define mid (+ (quotient n 2) 1))
        (define nleft (- mid 1))
        (define nright (- n mid))
        (define e (lselect mid l))
        (define left (lleft nleft l))
        (define right (lright nright l))
        (define result (list left e right)))
       (begin
;         (display l) (display "->")
;         (display left) (display " ")
;         ;(display " ") (display " n ") (display n)
;         (display "[") (display mid) (display "]:") (display e) (display " ")
;         ;(display " l ") (display nleft)
;         ;(display " r ") (display nright)
;         (display right)
;         (newline)
         result)))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l null)
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (local
           ((define l (list 'a))
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (local
           ((define l (list 'a 'b))
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c))
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd))
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd 'e))
            (define result (lsplit l)))
           (begin
             (display "list ") (display l) (newline)
             (display "lsplit ") (display result) (newline)))
         (newline)
         (newline))))
 "Test9" show?)

; create a list in the following order:
; (append (cons mid (produce left) (produce right))
; (cons mid (produce left (produce right null))
; this last version is without append!

(define (balanced-order l)
  (cond
    ((null? l) null)
    (else
     (local
       ((define (recurse l result)
          (local
            ((define split (lsplit l))
             (define e (cadr split))
             (define left (car split))
             (define right (caddr split))
             (define res+e (cons e result)))
            (begin
              ;(display "result:") (display result) (display " ")
              ;(display "l:") (display l) (display "->")
              ;(display left) (display " ")
              ;(display e) (display " ")
              ;(display right) (newline)
              (cond
                ((and (null? left) (null? right)) res+e)
                ((null? right) (recurse left res+e))
                ((null? left) (recurse right res+e))
                (else ; both left and right are not null
                 (recurse right (recurse left res+e)))))))
        (define res-reverse (recurse l null))
        (define result (foldl cons null res-reverse)))
       (begin
         ;(display "result:") (display res-reverse) (newline)
         ;(display result) (newline)
       result)))))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l null)
            (define result (balanced-order l)))
           (begin
             (display "list ") (display l) (newline)
             (display "balanced-order ") (display result) (newline)))
         (local
           ((define l (list 'a))
            (define result (balanced-order l)))
           (begin
             (display "list ") (display l) (newline)
             (display "balanced-order ") (display result) (newline)))
         (local
           ((define l (list 'a 'b 'c 'd 'e))
            (define result (balanced-order l)))
           (begin
             (display "list ") (display l) (newline)
             (display "balanced-order ") (display result) (newline)))
         (local
           ((define l (list 1 2 3 4 5 6 7 8 9 10 11))
            (define result (balanced-order l)))
           (begin
             (display "list ") (display l) (newline)
             (display "balanced-order ") (display result) (newline)))
         (newline)
         (newline))))
 "Test10" show?)

(define (list->btree l)
  (foldl adjoin-set null (balanced-order l)))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define l (list 1 2 3 4 5 6 7 8 9 10 11))
            (define result (btree->list (list->btree l))))
           (begin
             (display "list ") (display l) (newline)
             (display "btree->list ") (display result) (newline)))
         (local
           ((define l (list 1 2 3 4 5 6 7 8 9 10 11))
            (define result (btree->list2 (list->btree l))))
           (begin
             (display "list ") (display l) (newline)
             (display "btree->list2 ") (display result) (newline)))
         (newline)
         (newline))))
 "Test11" show?)

; union is implemented by unrolling btree to an ordered list
; and taking union of ordered list which is O(n)
(define (union-set s1 s2)
  (local
    ((define (union-set-l l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        (else
         (local
           ((define x1 (car l1))
            (define x2 (car l2)))
           (cond
             ((= x1 x2) (cons x1 (union-set-l (cdr l1) (cdr l2))))
             ((< x1 x2) (cons x1 (union-set-l (cdr l1) l2)))
             (else (cons x2 (union-set-l l1 (cdr l2)))))))))
    (define l1 (btree->list s1))
    (define l2 (btree->list s2))
    (define l (union-set-l l1 l2))
    (define result (list->btree l)))
    result))

((λ (name show?)
   (if (not show?)
       (void)
       (begin
         (display name) (newline)
         (local
           ((define s1 (list->btree (list 1 3 5 25 27)))
            (define s2 (list->btree (list -3 5 22 100 123))))
           (begin
             (display "s1 ") (display-btree s1) (newline)
             (display "s2 ") (display-btree s2) (newline)
             (local
               ((define res (union-set s1 s2)))
               (begin
                 (display "union ") (display-btree res) (newline)))))
         (newline)
         (newline))))
 "Test12" show?)

; union is implemented by unrolling btree to an ordered list
; and taking intersection of ordered list which is O(n)
(define (intersection-set s1 s2)
  (local
    ((define (intersection-set-l l1 l2)
       (cond
         ((or (null? l1) (null? l2)) null)
         (else
          (local
            ((define x1 (car l1))
             (define x2 (car l2)))
            (cond
              ((= x1 x2)
               (cons x1 (intersection-set-l (cdr l1) (cdr l2))))
              ((< x1 x2) (intersection-set-l (cdr l1) l2))
              (else
               (intersection-set-l l1 (cdr l2))))))))
     (define l1 (btree->list s1))
     (define l2 (btree->list s2))
     (define l (intersection-set-l l1 l2))
     (define result (list->btree l)))
    result))

((λ (name show?)
   (cond
     ((not show?) (void))
     (else
      (begin
        (display name) (newline)
        (local
          ((define set1 (list->btree (list 1 3 6 10)))
           (define set2 (list->btree (list 2 3 4 5 6))))
          (begin
            (display "set1 ") (display-btree set1) (newline)
            (display "set2 ") (display-btree set2) (newline)
            (local
              ((define res (intersection-set set1 set2)))
              (begin
                (display "intersection ") (display-btree res) (newline)))
            (newline)
            (newline)))))))
 "Test13" show?)