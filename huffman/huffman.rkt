#lang racket

(local
  ((define description
#<<here-string

2.3.4 Example: Huffman Encoding Trees
This section provides practice in the use of list structure
and data abstraction to manipulate sets and trees. The ap-
plication is to methods for representing data as sequences
of ones and zeros (bits).

here-string
   ))
  (begin
    (display description)
    (newline)
    (newline)))

(define (make-htree w set left right)
  (list w set left right))

(define (htree-w htree)
  (car htree))

(define (htree-set htree)
  (cadr htree))

(define (htree-left htree)
  (caddr htree))

(define (htree-right htree)
  (cadddr htree))

; set is represented as a list without duplicates

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (foldl (λ (e result)
           (if (element-of-set? e result)
               result
               (cons e result)))
         s2
         s1))

(define (adjoin-set e set)
  (cond
    ((null? set) (list e))
    ((equal? e (car set)) set)
    (else (cons (car set) (adjoin-set e (cdr set))))))

; list insert with keeping order
; with respect to weight
; keep duplicates
(define (linsert x l)
  (cond
    ((null? l) (list x))
    (else
     (local
       ((define x2 (car l)))
       (cond
         ((< (htree-w x) (htree-w x2)) (cons x l))
         ((= (htree-w x) (htree-w x2)) (cons x l))
         (else (cons x2 (linsert x (cdr l)))))))))

; order list
(define (lorder l)
  (foldl linsert null l))


(define (initail-htree-list msg)
  (local
    ((define message (string->list msg)))
     ; order result with regards
     ; to weights
     (lorder
      ; count occurences of symbols
      ; in a message
      (map (λ(s)
             (local
               ((define (iterate n msg)
                  (cond
                    ((null? msg) n)
                    ((equal? s (car msg)) (iterate (+ n 1) (cdr msg)))
                    (else (iterate n (cdr msg)))))
                (define w (iterate 0 message))
                (define set (adjoin-set s null)))
               ; create h-tree from a count and symbol
               (make-htree w set null null)))
           ; create a set of symbols from message
           (foldl adjoin-set null message)))))

(define (htree-merge htree1 htree2)
  (cond
    ((null? htree1) htree2)
    ((null? htree2) htree1)
    (else
     (local
       ((define w1 (htree-w htree1))
        (define w2 (htree-w htree2))
        (define w (+ w1 w2))
        (define s1 (htree-set htree1))
        (define s2 (htree-set htree2))
        (define s (union-set s1 s2)))
       (cond
         ((<= w1 w2) (make-htree w s htree1 htree2))
         (else (make-htree w s htree2 htree1)))))))


(define (create-htree msg)
  (local
    ((define inital (initail-htree-list msg))
     (define (iterate list)
       (cond
         ((null? (cdr list)) (car list))
         (else
          (local
            ((define htree1 (car list))
             (define htree2 (cadr list))
             (define list2 (cddr list))
             (define htree (htree-merge htree1 htree2)))
            (iterate (linsert htree list2)))))))
    (iterate inital)))

(define (display-set set)
  (local
    ((define (iterate set)
       (cond
         ((null? set) (void))
         (else
          (begin
            (display (car set)) (display " ")
            (iterate (cdr set)))))))
    (begin
      (display "{ ")
      (iterate set)
      (display "}"))))

(define (display-htree htree)
  (local
    ((define (iterate htree encoding)
       (local
         ((define w (htree-w htree))
          (define s (htree-set htree))
          (define left (htree-left htree))
          (define right (htree-right htree)))
         (begin
           (for ((i encoding)) (display i)) (display " ")
           (display w) (display " ") (display-set s) (newline)
           (if (null? left)
               (void)
               (iterate left (foldr cons (list 0) encoding)))
           (if (null? right)
               (void)
               (iterate right (foldr cons (list 1) encoding)))))))
    (iterate htree null)))

((λ (name)
   (begin
     (display name)
     (newline)
     (local
       ((define message "BACA")
        (define htree (create-htree message)))
       (begin
         (display "message ") (display message) (newline)
         (display "huffman tree") (newline) (display-htree htree) (newline)))
     (local
       ((define message "BACADAEAFABBAAAGAH")
        (define htree (create-htree message)))
       (begin
         (display "message ") (display message) (newline)
         (display "huffman tree") (newline) (display-htree htree) (newline)))
     (newline)))
 "Test1")

(local
  ((define description
#<<here-string

The decoding procedure

The following procedure implements the decoding algo-
rithm. It takes as arguments a list of zeros and ones,
together with a Huffman tree.

here-string
   ))
  (begin
    (display description)
    (newline)
    (newline)))  
 
(define (decode-huff message htree)
  (local
    ((define l (string->list message))
     (define (iterate message result ht)
       (local
         ((define set (htree-set ht))
          (define left (htree-left ht))
          (define right (htree-right ht)))
         (cond
           ((or (null? left)
                (null? right))
            (local
              ((define res+sym (cons (car set) result)))
              (if (null? message)
                  res+sym
                  (iterate message res+sym htree))))
           ((null? message)
            (begin
              (display "Error: failed to decode the message, partial result ")
              (display (list->string result))
              (newline)
              false))        
           (else
            (local
              ((define x (car message)))
              (cond
                ((equal? x #\0)
                 (iterate  (cdr message) result left))
                ((equal? x #\1)
                 (iterate (cdr message) result right))
                (else
                 (begin
                   (display "Error: unknown symbol in the message ") (display x)
                   (newline)
                   false))))))))
     (define result (iterate (string->list message) null htree)))
    (if (equal? result false)
        false
        (list->string (foldl cons null result)))))

((λ (name)
   (begin
     (display name)
     (newline)
     (local
       ((define message "BACADAEAFABBAAAGAH")
        (define htree (create-htree message)))
       (begin
         (display "message ") (display message) (newline)
         (display "huffman tree") (newline) (display-htree htree) (newline)
         (local
           ((define encoded-msg "1011000100000011001001010100")
            (define decoded-msg (decode-huff encoded-msg htree)))
           (if (equal? decoded-msg false)
               (void)
               (begin
                 (display "encoded msg ") (display encoded-msg) (newline)
                 (display "decoded msg ") (display decoded-msg) (newline))))
         (local
           ((define encoded-msg "001000110000011")
            (define decoded-msg (decode-huff encoded-msg htree)))
           (if (equal? decoded-msg false)
               (void)
               (begin
                 (display "encoded msg ") (display encoded-msg) (newline)
                 (display "decoded msg ") (display decoded-msg) (newline))))
         (local
           ((define encoded-msg "00100011000001")
            (define decoded-msg (decode-huff encoded-msg htree)))
           (if (equal? decoded-msg false)
               (void)
               (begin
                 (display "encoded msg ") (display encoded-msg) (newline)
                 (display "decoded msg ") (display decoded-msg) (newline))))))
     (newline)))
 "Test2")

(define (encode-huff message htree)
  (local
    ((define msg (string->list message))
     (define (iterate m ht)
       (cond
         ((null? m) null)
         (else
          (local
            ((define set (htree-set ht))
             (define left (htree-left ht))
             (define right (htree-right ht))
             (define x (car m))
             (define (error-msg show?)
               (if show?
                   (begin
                     (display "Error: symbol ") (display x)
                     (display " in message ") (display message)
                     (display " is not recognized") (newline)
                     false)
                   (void))))
            (cond
              ((and (null? left)
                    (null? right)
                    (equal? x (car set)))
               (cons x (iterate (cdr m) htree)))
              ((element-of-set? x set)
               (cond
                 ((element-of-set? x (htree-set left)) (iterate m left))
                 ((element-of-set? x (htree-set right)) (iterate m right))
                 (else (error-msg true))))
              (else (error-msg true)))))))
     (define result (iterate msg htree)))
    (if (equal? result false)
        false
        (list->string result))))

((λ (name)
   (begin
     (display name)
     (newline)
     (local
       ((define message "BACADAEAFABBAAAGAH")
        (define htree (create-htree message)))
       (begin
         (display "message ") (display message) (newline)
         (display "huffman tree") (newline) (display-htree htree) (newline)
         (local
           ((define msg "1011000100000011001001010100")
            (define decoded-msg (decode-huff msg htree))
            (define encoded-msg (encode-huff decoded-msg htree)))
           (begin
             (if (equal? decoded-msg false)
               (void)
               (begin
                 (display "msg ") (display msg) (newline)
                 (display "decoded msg ") (display decoded-msg) (newline)))
             (if (equal? encoded-msg false)
               (void)
               (begin
                 (display "encoded-msg ") (display encoded-msg) (newline)))))
         (local
           ((define msg "10110001000000110010010100")
            (define encoded-msg (encode-huff msg htree)))
           (begin
             (if (equal? encoded-msg false)
               (void)
               (begin
                 (display "encoded-msg ") (display encoded-msg) (newline))))))
     (newline))))
 "Test3")

             


                