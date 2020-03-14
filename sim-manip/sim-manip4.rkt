#lang racket

(define show? true)

(define (descr show?)
  (if (not show?)
      (void)
      (local
        ((define descr
#<<here-string


Exercise 2.58: Suppose we want to modify the differentiation
program so that it works with ordinary
mathematical notation, in which + and * are infix
rather than prefix operators. Since the differentiation
program is defined in terms of abstract data, we
can modify it to work with different representations
of expressions solely by changing the predicates, selectors,
and constructors that define the representation
of the algebraic expressions on which the differentiator
is to operate.

b. The problem becomes substantially harder if we
allow standard algebraic notation, such as (x + 3 * (x + y + 2)),
which drops unnecessary parentheses and assumes that multiplication is
done before addition. Can you design appropriate predicates, selectors,
and constructors for this notation such that our derivative program still works?


here-string
           ))
        (begin
          (display descr)
          (newline)
          (newline)))))

(descr show?)

(define (derivative expr var)
  (local
    ; d(u+v)/dx=du/dx+dv/dx
    ((define (sum-deriv u v)
       (local
         ((define der1 (recurse u))
          (define der2 (recurse v))
          (define is-error?
            (or (equal? der1 false)
                (equal? der2 false))))
         (cond
           (is-error? false)
           (else (sum-make der1 der2)))))
     ; d(uv)/dx=udv/dx+vdu/dx
     (define (prod-deriv u v)
       (local
         ((define der1 (recurse u))
          (define der2 (recurse v))
          (define is-error?
            (or (equal? der1 false)
                (equal? der2 false))))
         (cond
           (is-error? false)
           (else 
            (sum-make
             (prod-make u der2)
             (prod-make v der1))))))
     ; d(u^n)/dx = nu^n-1du/dx
     (define (exp-der base n)
       (local
         ((define der (recurse base))
          (define is-error? (equal? der false))
          ; check if exponent is a list and strip list
          (define n2 (if (pair? n) (car n) n)))
         (if is-error?
             false      
             (prod-make n (prod-make
                           (exp-make base (- n2 1))
                           der)))))
     (define (recurse expr)      
       (cond
         ; dc/dx=0
         ((is-const-expr? expr var) make-0)
         ; dx/dx=1
         ((is-var-expr? expr var) make-1)
         (else
          (local
            ((define split (get-split expr)))
            (if (equal? split false)
                false
                (local
                  ((define op (get-split-op split))
                   (define lhs (get-split-lhs split))
                   (define rhs (get-split-rhs split)))
                  (cond
                    ((is-sum-op? op) (sum-deriv lhs rhs))
                    ((is-prod-op? op) (prod-deriv lhs rhs))
                    ((is-exp-op? op) (exp-der lhs rhs))
                    (else false)))))))))
    (recurse expr)))

(define (is-sum-op? op) (equal? op '+))
(define (sum-make u v)
  (cond
    ((and (number? u) (number? v)) (+ u v))
    ((equal? u 0) v)
    ((equal? v 0) u)
    ((equal? u v) (list 2 '* u))
    (else (list u '+ v))))
(define (is-prod-op? op) (equal? op '*))
(define (prod-make u v)
  (cond
    ((and (number? u) (number? v)) (* u v))
    ((equal? u 1) v)
    ((equal? v 1) u)
    ((equal? u 0) 0)
    ((equal? v 0) 0)
    (else (list u '* v))))
(define make-0 '0)
(define make-1 '1)
(define (is-exp-op? op) (or (equal? op '**) (equal? op '^)))
(define (exp-make base n)
  (cond
    ((and (number? base) (number? n)) (expt base n))
    ((equal? n 0) 1)
    ((equal? n 1) base)
    (else (list base '** n))))

(define (get-op1 expr) (car expr))
(define (get-lhs expr) (list (get-op1 expr)))
;  (local
;    ((define op1 (get-op1 expr)))
;    (if (pair? op1) op1 (list op1))))
(define (get-op expr)
  (cond
   ((null? (cdr expr)) null)
   (else (car (cdr expr)))))
; case 1: expr (a + b + c + d), op2 is (b + c + d) (cdr (cdr expr))
; case 2: (a + (b + c) + d) op2 is ((b+c) + d) (cdr (cdr expr))
; case 3: (a + b) op2 is b (car (cdr (cdr expr)))
; we need to check if (cdr (cdr (cdr expr))) is null to differentiate 3 from 1 and 2
(define (get-rhs expr)
  (cond
    ((null? (get-op expr)) null)
    (else
     (cdr (cdr expr)))))
(define (get-op2 expr)
  (local
    ((define op2 (get-rhs expr)))
    (if (null? (cdr op2)) (car op2) op2)))
; Returns boolean on success, false and prints error message (side effect) on fail
(define (valid-op? op)
  (cond
    ((or (is-sum-op? op) (is-exp-op? op) (is-prod-op? op)) true)
    (else
     (begin
       (display "Error: unknown operation ")
       (display op)
       (newline)
       false))))
(define (weaker-op? this-op op)
  (cond
    ((is-sum-op? this-op) (or (is-exp-op? op)
                              (is-prod-op? op)))
    ((is-prod-op? this-op) (is-exp-op? op))
    ; (is-exp-op? this-op)
    (else false)))
; Look for the weakast operation op in expression
; and returns (lhs-op op rhs-op)
(define (make-split lhs op rhs) (list lhs op rhs))
(define (get-split-op split) (car (cdr split)))
(define (get-split-lhs split) (remove-double-parant (car split)))
(define (get-split-rhs split) (remove-double-parant (car (cdr (cdr split)))))
; and convert to (expr) (cons expr null)
(define (remove-double-parant expr)
  (if (and (pair? expr) (null? (cdr expr)) (pair? (car expr))) (car expr) expr))
(define (get-split expr)
  (local
    ((define (iterate split lhs op rhs)
       (cond
         ((null? op) split)
         ((not (valid-op? op)) false)
         (else
          (local
            ((define new-lhs (append lhs (list op) (get-lhs rhs)))
             (define new-op (get-op rhs))
             (define new-rhs (get-rhs rhs)))
            (cond
              ((or (null? split)
                   (local
                     ((define split-op (get-split-op split)))
                     (weaker-op? op split-op)))
               (iterate (make-split lhs op rhs) new-lhs new-op new-rhs))
              (else (iterate split new-lhs new-op new-rhs))))))))
     (iterate null (get-lhs expr) (get-op expr) (get-rhs expr))))

(define (is-const-expr? expr var)
  (cond
    ((not (pair? expr)) (not (equal? expr var)))
    ((empty? expr) true)
    (else (and
           (is-const-expr? (car expr) var)
           (is-const-expr? (cdr expr) var)))))
; Test for x or (x)
(define (is-var-expr? expr var)
  (or (and (not (pair? expr))
           (equal? expr var))
      (and (pair? expr)
           (null? (cdr expr))
           (equal? (car expr) var))))

((λ (test-name show?)
   (cond
     ((not show?) (void))
     (else
      (begin
        (display test-name) (newline)
        (local
          ((define expr '((x + y) * h + (x + y) * z + k)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (void)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a * b + c + d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a * b * c + d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a * b * c * d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a ^ b * c ^ d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a ^ b * c + d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a ^ b % c + d)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(x + (x + y))))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(((a * (x ** 2)) + (b * x)) + c)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '((a * (x ** 2)) + (b * x) + c)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '((a * (x ^ 2)) + (b * x) + c)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(a * x ^ 2 + b * x + c)))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '(x + 3 * (x + y + 2))))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (local
          ((define expr '((x + y) + (z + w))))
          (begin
            (display "expression ") (display expr) (newline)
            (local
              ((define res (get-split expr)))
              (if (equal? res false)
                  (newline)
                  (local
                    ((define lhs (get-split-lhs res))
                     (define op (get-split-op res))
                     (define rhs (get-split-rhs res)))
                    (begin
                      (display "split: ") (display res) (newline)
                      (display "lhs: ") (display lhs) (newline)
                      (display "op: ") (display op) (newline)
                      (display "rhs: ") (display rhs) (newline)
                      (newline)))))))
        (begin
          (display "*******************\n\n"))))))
 "Test0" show?)

(define (test1 show?)
  (cond
    (show?
     (local
       ((define expr '((x % 2) + 3))
        (define var 'x))
       (begin
         (display "Test1\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test1 show?)

(define (test2 show?)
  (cond
    (show?
     (local
       ((define expr 'y)
        (define var 'x))
       (begin
         (display "Test2\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test2 show?)

(define (test3 show?)
  (cond
    (show?
     (local
       ((define expr '(a * y))
        (define var 'x))
       (begin
         (display "Test3\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test3 show?)

(define (test4 show?)
  (cond
    (show?
     (local
       ((define expr 'x)
        (define var 'x))
       (begin
         (display "Test4\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test4 show?)

(define (test5 show?)
  (cond
    (show?
     (local
       ((define expr '(x + x))
        (define var 'x))
       (begin
         (display "Test5\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test5 show?)

(define (test6 show?)
  (cond
    (show?
     (local
       ((define expr1 '(x + (x + y)))
        (define expr2 '(x + x + y))
        (define var 'x))
       (begin
         (display "Test6\n")
         (display "expression ") (display expr1) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr1 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr2) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr2 var)))
           (if (equal? res false) (void) (display res)))         
         (newline)
         (newline))))
    (else (void))))

(test6 show?)

(define (test7 show?)
  (cond
    (show?
     (local
       ((define expr1  '((y + y) + (x + x)))
        (define expr2  '(y + y + x + x))
        (define var 'x))
       (begin
         (display "Test7\n")
         (display "expression ") (display expr1) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr1 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr2) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr2 var)))
           (if (equal? res false) (void) (display res)))         
         (newline)
         (newline))))
    (else (void))))

(test7 show?)

(define (test8 show?)
  (cond
    (show?
     (local
       ((define expr '(x * x))
        (define var 'x))
       (begin
         (display "Test8\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test8 show?)

(define (test9 show?)
  (cond
    (show?
     (local
       ((define expr '(x + 3))
        (define var 'x))
       (begin
         (display "Test9\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test9 show?)

(define (test10 show?)
  (cond
    (show?
     (local
       ((define expr '(x * y))
        (define var 'x))
       (begin
         (display "Test10\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test10 show?)

(define (test11 show?)
  (cond
    (show?
     (local
       ((define expr '((x * y) * (x + 3)))
        (define expr2 '(x * y * (x + 3)))
        (define var 'x))
       (begin
         (display "Test11\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr2) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr2 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test11 show?)

(define (test12 show?)
  (cond
    (show?
     (local
       ((define expr1 '(((a * (x * x)) + (b * x)) + c))
        (define expr2 '((a * x * x) + (b * x) + c))
        (define var 'x))
       (begin
         (display "Test12\n")
         (display "expression ") (display expr1) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr1 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr2) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr2 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test12 show?)

(define (test13 show?)
  (cond
    (show?
     (local
       ((define expr1 '(((a * (x ** 2)) + (b * x)) + c))
        (define expr2 '((a * (x ** 2)) + (b * x) + c))
        (define expr3 '((a * (x ^ 2)) + (b * x) + c))
        (define expr4 '(a * x ^ 2 + b * x + c))
        (define var 'x))
       (begin
         (display "Test13\n")
         (display "expression ") (display expr1) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr1 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr2) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr2 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr3) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr3 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (display "expression ") (display expr4) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr4 var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test13 show?)


