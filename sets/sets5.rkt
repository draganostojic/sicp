#lang racket

(begin
  (local
    ((define description
#<<here-string
locate the record with a given key we use a procedure lookup,
which takes as arguments a key and a data base and which
returns the record that has that key, or false if there is no
such record.

Exercise 2.66: Implement the lookup procedure for
the case where the set of records is structured as a
binary tree, ordered by the numerical values of the
keys.
here-string
       ))
    (begin
      (display description)
      (newline)
      (newline))))

; node is (list key record)
(define (make-node key record)
  (list key record))

; btree is (list node left right)
(define (make-btree node left right)
  (list node left right))

(define (btree-node btree)
  (cond
    ((null? btree) null)
    (else (car btree))))

(define (btree-left btree)
  (cond
    ((null? btree) null)
    (else (cadr btree))))

(define (btree-right btree)
  (cond
    ((null? btree) null)
    (else (caddr btree))))

(define (node-key node)
  (car node))

(define (node-record node)
  (cadr node))

(define (display-db db)
  (local
    ((define (iterate btree)
       (cond
         ((null? btree) (void))
         (else
          (begin
            (iterate (btree-left btree))
            (display "[ ")
            (display (node-key (btree-node btree))) (display ",")
            (display (node-record (btree-node btree))) (display " ]") (newline)
            (iterate (btree-right btree)))))))
    (iterate db)))

(define (adjoin-set x set)
  (cond
    ((null? set) (make-btree x null null))
    (else
     (local
       ((define node (btree-node set))
        (define left (btree-left set))
        (define right (btree-right set)))
       (cond    
         ((= (node-key x) (node-key node)) set)
         ((< (node-key x) (node-key node)) (make-btree node (adjoin-set x left) right))
         (else (make-btree node left (adjoin-set x right))))))))

(define (db-add key record db)
  (adjoin-set (make-node key record) db))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    (else
     (local
       ((define node (btree-node set))
        (define left (btree-left set))
        (define right (btree-right set)))
       (cond
         ((< (node-key x) (node-key node)) (element-of-set? x left))
         ((= (node-key x) (node-key node)) (node-record node))
         (else (element-of-set? x right)))))))

(define (lookup key db)
  (element-of-set? (make-node key null) db))

((λ (name)
   (begin
     (display name) (newline)
     (local
       ((define db
             (db-add 500 (list "Dragan" "123-345")
             (db-add 250 (list "Vesna" "678")
             (db-add 1000 (list "Nikola" "777788888")
             (db-add 3 (list "Sofija" "hjshdjhsjd")
             (db-add 88 (list "Bob" "74")
             (db-add 300 (list "Allice" "8989sdfsdf")
             (db-add 7 (list "Charlie" "iusdiofoaiud")
             (db-add 88 (list "David" "iaosdiuofaodio")
             (db-add 923 (list "Ellen" "22333333")
             (db-add 10234 (list "Falcon" "ajdhfkahkjdh")
             (db-add 11 (list "George" "89897asdfasd")
             (db-add 1234 (list "Hellen" "akjsdkfkahj----afsdfasd")
             (db-add 134 (list "Ilya" "akjshdkfajh99999")
                     null))))))))))))))
        (define key 88))
       (begin
         (display "db") (newline)
         (display-db db) (newline)
         (display "key ") (display key) (display " in db? ")
         (local
           ((define record (lookup key db)))
           (if (equal? record false)
               (begin
                 (display "no") (newline))
               (begin
                 (display "yes ") (display "record ") (display record) (newline))))))
     (local
       ((define db
             (db-add 500 (list "Dragan" "123-345")
             (db-add 250 (list "Vesna" "678")
             (db-add 1000 (list "Nikola" "777788888")
             (db-add 3 (list "Sofija" "hjshdjhsjd")
             (db-add 88 (list "Bob" "74")
             (db-add 300 (list "Allice" "8989sdfsdf")
             (db-add 7 (list "Charlie" "iusdiofoaiud")
             (db-add 88 (list "David" "iaosdiuofaodio")
             (db-add 923 (list "Ellen" "22333333")
             (db-add 10234 (list "Falcon" "ajdhfkahkjdh")
             (db-add 11 (list "George" "89897asdfasd")
             (db-add 1234 (list "Hellen" "akjsdkfkahj----afsdfasd")
             (db-add 134 (list "Ilya" "akjshdkfajh99999")
                     null))))))))))))))
        (define key -1))
       (begin
         (display "db") (newline)
         (display-db db) (newline)
         (display "key ") (display key) (display " in db? ")
         (local
           ((define record (lookup key db)))
           (if (equal? record false)
               (begin
                 (display "no") (newline))
               (begin
                 (display "yes ") (display "record ") (display record) (newline))))))
     (newline)
     (newline)))
 "Test1")

(define (lright n l)
   (cdr
    (foldr
     (λ (x acc)
       (if (>= (car acc) n)
           acc
           (cons (+ (car acc) 1) (cons x (cdr acc)))))
     (cons 0 null)
     l)))

(define (lleft n l)
     (local
     ((define (recurse i l)
        (cond
          ((null? l) null)
          ((>= i n) null)
          (else (cons (car l) (recurse (+ i 1) (cdr l)))))))
     (recurse 0 l)))

(define (lsize l)
  (foldr (λ (x acc) (+ 1 acc)) 0 l))

(define (lselect n l)
  (local
    ((define (iterate i l)
       (cond
         ((= i n) (car l))
         (else (iterate (+ i 1) (cdr l))))))
    (iterate 1 l)))

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
         result)))))

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
              (cond
                ((and (null? left) (null? right)) res+e)
                ((null? right) (recurse left res+e))
                ((null? left) (recurse right res+e))
                (else ; both left and right are not null
                 (recurse right (recurse left res+e)))))))
        (define res-reverse (recurse l null))
        (define result (foldl cons null res-reverse)))
       (begin
       result)))))

; produces balanced tree from a list
(define (list->btree l)
  (foldl adjoin-set null (balanced-order l)))

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
              (cond
                ((and (null? lbt) (null? rbt)) (cons node result))
                ((null? lbt) (cons node (recurse rbt result)))
                ((null? rbt) (recurse lbt (cons node result)))
                (else (recurse lbt (cons node (recurse rbt result))))))))
        (define result (recurse btree null)))
       result))))

; list insert with keeping order
(define (lkeyinsert x l)
  (cond
    ((null? l) (list x))
    (else
     (local
       ((define x2 (car l)))
       (cond
         ((< (node-key x) (node-key x2)) (cons x l))
         ((= (node-key x) (node-key x2)) l)
         (else (cons x2 (lkeyinsert x (cdr l)))))))))

; order list
(define (lkeyorder l)
  (foldl lkeyinsert null l))

(define (list->db l)
  (list->btree (lkeyorder l)))

((λ (name)
   (begin
     (display name) (newline)
     (local
       ((define l
          (list
           (make-node 500 (list "Dragan" "123-345"))
           (make-node 250 (list "Vesna" "678"))
           (make-node 1000 (list "Nikola" "777788888"))
           (make-node 3 (list "Sofija" "hjshdjhsjd"))
           (make-node 88 (list "Bob" "74"))
           (make-node 300 (list "Allice" "8989sdfsdf"))
           (make-node 7 (list "Charlie" "iusdiofoaiud"))
           (make-node 88 (list "David" "iaosdiuofaodio"))
           (make-node 923 (list "Ellen" "22333333"))
           (make-node 10234 (list "Falcon" "ajdhfkahkjdh"))
           (make-node 11 (list "George" "89897asdfasd"))
           (make-node 1234 (list "Hellen" "akjsdkfkahj----afsdfasd"))
           (make-node 134 (list "Ilya" "akjshdkfajh99999"))))
        (define db (list->db l)))
       (begin
         (local
           ((define key 88))
           (begin
             (display "db") (newline)
             (display-db db) (newline)
             (display "key ") (display key) (display " in db? ")
             (local
               ((define record (lookup key db)))
               (if (equal? record false)
                   (begin
                     (display "no") (newline))
                   (begin
                     (display "yes ") (display "record ") (display record) (newline))))))
         (local
           ((define key -1))
           (begin
             (display "db") (newline)
             (display-db db) (newline)
             (display "key ") (display key) (display " in db? ")
             (local
               ((define record (lookup key db)))
               (if (equal? record false)
                   (begin
                     (display "no") (newline))
                   (begin
                     (display "yes ") (display "record ") (display record) (newline))))))))
     (newline)
     (newline)))
 "Test2")