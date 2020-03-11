#lang racket

;;
(define (length l)
  (local
    ((define (iterate n l)
       (if (null? l) n (iterate (+ n 1) (rest l)))))
    (iterate 0 l)))

(define odds (list 1 3 5 7))

(length odds)

;;
(define (append2 l1 l2)
  (cond
    ((null? l1) l2)
    (else (cons (first l1) (append2 (rest l1) l2)))))

(append2 (list 1 2 3) (list 4 5 6))

; Exercise 2.17

(define (last-pair l)
  (cond
    ((null? l) false)
    ((null? (rest l)) (first l))
    (else (last-pair (rest l)))))

(last-pair (list 1 2 3 4 5))

; Exercise 2.18

(define (reverse2 l)
  (local
    ((define (iterate acc l)
       (cond
         ((null? l) acc)
         (else (iterate (cons (first l) acc) (rest l))))))
    (iterate null l)))

(reverse2 (list 1 2 3 4 5))

; Exercise 2.19
; l = list of change
; n = amount

(define (cc l n)
  (cond
    ((< n 0) 0)
    ((null? l) 0)
    ((= n 0) 1)
    (else (+ (cc (rest l) n) (cc l (- n (first l)))))))

(require test-engine/racket-tests)

(check-expect (cc (list 50 25 10 5 1) 100) 292)

(test)

(cc (list 100 50 20 10 5 2 1 0.5) 100)



; Exercise 2.20
; Variable number of arguments function

; At least one argument

(define (f x . y)
  (begin
    (display x) (newline)
    (display y) (newline)))

(f 1 2 3 4 5 6)

(f 1)

; zero or more arguments

(define (f1 . x)
  (begin
    (display x) (newline)))

(f1 1 2 3)
(f1 4)
(f1)

; zero or more using lambda notation

(define f2 (lambda x
             (begin
               (display x) (newline))))

(f2 1 2 3)
(f2 1)
(f2)

(define (same-parity . x)
  (cond
    ((null? x) '())
    ((null? (rest x)) x)
    (else
     (local
       ((define parity (remainder (first x) 2)))
       (define (iterate result x)
         (cond
           ((null? x) result)
           ((= parity (remainder (first x) 2)) (iterate (cons (first x) result) (rest x)))
           (else (iterate result (rest x)))))
       (iterate '() x)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 1)
(same-parity)

(define (same-parity2 . x)
  (cond
    ((null? x) '())
    ((null? (rest x)) (first x))
    (else
     (local
      ((define p (remainder (first x) 2))
       (define (iterate l)
         (cond
           ((null? l) '())
           ((= (remainder (first l) 2) p) (cons (first l) (iterate (rest l))))
           (else (iterate (rest l))))))
      (iterate x)))))

(same-parity2 1 2 3 4 5 6 7)
(same-parity2 2 3 4 5 6 7)
(same-parity2 1)
(same-parity2)


; Mapping over lists

; mep operations per lists element are independent so we should think about map as working on list
; elements at the same time (instead of being sequential)

; thining about map as performing operations on all list elements at the same tiome i.e. operating on
; lists as single entities instead of thinking as being operated seuqentially is a next level of
; abstration

; map can be used to implement vector operations like scaler multiplication, vector addition,
; dot product etc

; in the contect of vector operations, map generalizes scaler operations to vector operations the
; same way lists generalize numbers to vectors

; more importantly, map can be implemented to perform vector operations in parallel


(define v1 (list 1 2 3))
(define v2 (list 4 5 6))
(define v3 (list 7 8 9))

(define (vec-add x y . l)
  (begin
    ;(display "x=") (display x) (newline)
    ;(display "y=") (display y) (newline)
    ;(display "l=") (display l) (newline)
    (apply map + (append (list x y) l))))

(display v1) (display "+") (display v2) (display "+") (display v3) (newline)
(vec-add v1 v2 v3)
(display v1) (display "+") (display v2) (newline)
(vec-add v1 v2)

; NOTE:
; When variable list of args is provided to another function that accepts variable list
; it must be done via apply whose first argument is function and the second arg is list of arguments

(define (dot-prod x y)
  (foldr + 0 (map * x y)))

(display v1) (display ".") (display v2) (newline)
(dot-prod v1 v2)

(define (scal-prod s v)
  (map (lambda (x) (* s x)) v))

(define s 3)

(display s) (display "*") (display v1) (newline)
(scal-prod s v1)

(define (norm x)
  (sqrt (foldr + 0 (map (λ (x) (* x x)) x))))

(display "||") (display v1) (display "||") (newline)
(norm v1)

(display v1)(display ",")(display v2)(newline)
(for-each (λ (x y)
            (begin
              (display "x=") (display x)(display ",")(display "y=")(display y)(newline)))
          v1 v2)

(define (vec-dump x . l)
  (apply for-each (λ x
                    (local
                      ((define (iterate i x)
                         (cond
                           ((null? x) (newline))
                           (else
                            (begin
                              (display "x")(display i)(display "=")(display (car x))(display " ")
                              (iterate (+ i 1) (cdr x)))))))
                      (iterate 0 x)))
         (append (list x) l)))

(display v1)(newline)
(vec-dump v1)

(display v1)(display ",")(display v2)(newline)
(vec-dump v1 v2)

(display v1)(display ",")(display v2)(display ",")(display v3)(newline)
(vec-dump v1 v2 v3)

; Exercise 2.23

(define (for-each2 proc . l)
  (foldr (λ l (void)) (void) (apply map proc l)))

(display v1)(display ",")(display v2)(newline)
(for-each2 (λ (x y)
            (begin
              (display "x=") (display x)(display ",")(display "y=")(display y)(newline)))
          v1 v2)
