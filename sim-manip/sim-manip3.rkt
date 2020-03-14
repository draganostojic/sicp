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

a. Show how to do this in order to differentiate algebraic
expressions presented in infix form, such
as (x + (3 * (x + (y + 2)))). To simplify the
task, assume that + and * always take two arguments
and that expressions are fully parenthesized


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
          (define is-error? (equal? der false)))
         (if is-error?
             false      
             (prod-make n (prod-make
                           (exp-make base (- n 1))
                           der)))))
     (define (recurse expr)      
       (cond
         ; dc/dx=0
         ((is-const-expr? expr var) make-0)
         ; dx/dx=1
         ((is-var-expr? expr var) make-1)
         ((is-sum? expr)
          (sum-deriv (get-op1 expr) (get-op2 expr)))
         ((is-prod? expr)
          (prod-deriv (get-op1 expr) (get-op2 expr)))
         ((is-exp? expr)
          (exp-der (get-op1 expr) (get-op2 expr)))
         (else
          (local
            ((define error-msg "Error: unknown expression "))
            (begin
              (display error-msg) (display expr) (newline)
              false))))))
    (recurse expr)))

(define (get-op1 expr) (car expr))
(define (get-op expr) (car (cdr expr)))
; case 1: expr (a + b + c + d), op2 is (b + c + d) (cdr (cdr expr))
; case 2: (a + (b + c) + d) op2 is ((b+c) + d) (cdr (cdr expr))
; case 3: (a + b) op2 is b (car (cdr (cdr expr)))
; we need to check if (cdr (cdr (cdr expr))) is null to differentiate 3 from 1 and 2
(define (get-op2 expr)
  (local
    ((define op2 (cdr (cdr expr))))
    (if (null? (cdr op2)) (car op2) op2)))
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
(define (is-sum? expr) (equal? (get-op expr) '+))
(define (sum-make u v)
  (cond
    ((and (number? u) (number? v)) (+ u v))
    ((equal? u 0) v)
    ((equal? v 0) u)
    ((equal? u v) (list 2 '* u))
    (else (list u '+ v))))
(define (is-prod? expr) (equal? (get-op expr) '*))
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
(define (is-exp? expr) (or (equal? (get-op expr) '**)
                           (equal? (get-op expr) '^)))
(define (exp-make base n)
  (cond
    ((and (number? base) (number? n)) (expt base n))
    ((equal? n 0) 1)
    ((equal? n 1) base)
    (else (list base '** n))))

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
       ((define expr1  '((+ y y) + (x + x)))
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
         (newline))))
    (else (void))))

(test13 show?)

