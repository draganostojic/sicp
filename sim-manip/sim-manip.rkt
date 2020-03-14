#lang racket

(define show? true)

(define (show-descr show?)
  (local
    ((define descr
#<<here-string

2.3.2  Example:  Symbolic  DifferentiationAs an illustration of symbol
manipulation and a further il-lustration of data abstraction,
consider the design of a pro-cedure that performs symbolic differentiation
of algebraicexpressions.  We would like the procedure to take as arguments
an algebraic expression and a variable and to returnthe  derivative
of  the  expression  with  respect  to  the  vari-able.
For example, if the arguments to the procedure areax2+bx+candx,
the procedure should return 2ax+b.

Differentiation ofany such expression can be carried out by applying
the fol-lowing reduction rules:dc/dx=0,for c a constant or a variable
different from x,
dx/dx=1,d(u+v)/dx=du/dx+dv/dx,d(uv)/dx=udv/dx+vdu/dx.Observe
that  the  latter  two  rules  are  recursive  in  nature




here-string
       ))
    (if show? (display descr) (void))))

(show-descr show?)

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
     (define (recurse expr)      
       (cond
         ; dc/dx=0
         ((is-const-expr? expr var) make-0)
         ; dx/dx=1
         ((is-var-expr? expr var) make-1)
         ((is-sum? expr)
          (sum-deriv (sum-get-u expr) (sum-get-v expr)))
         ((is-prod? expr)
          (prod-deriv (prod-get-u expr) (prod-get-v expr)))
         (else
          (begin
            (display "Error: unknown expression ")
            (display expr) (newline)
            false)))))
    (recurse expr)))

(define (is-sum? expr) (equal? (car expr) '+))
(define (sum-get-u expr) (car (cdr expr)))
(define (sum-get-v expr) (car (cdr (cdr expr))))
(define (sum-make u v)
  (cond
    ((and (number? u) (number? v)) (+ u v))
    ((equal? u 0) v)
    ((equal? v 0) u)
    ((equal? u v) (list '* 2 u))
    (else (list '+ u v))))
(define (is-prod? expr) (equal? (car expr) '*))
(define (prod-get-u expr) (car (cdr expr)))
(define (prod-get-v expr) (car (cdr (cdr expr))))
(define (prod-make u v)
  (cond
    ((and (number? u) (number? v)) (* u v))
    ((equal? u 1) v)
    ((equal? v 1) u)
    ((equal? u 0) 0)
    ((equal? v 0) 0)
    (else (list '* u v))))
(define make-0 '0)
(define make-1 '1)
(define (is-const-expr? expr var)
  (cond
    ((not (pair? expr)) (not (equal? expr var)))
    ((empty? expr) true)
    (else (and
           (is-const-expr? (car expr) var)
           (is-const-expr? (cdr expr) var)))))
                                         
(define (is-var-expr? expr var)
  (cond
    ((not (pair? expr)) (equal? expr var))
    (else (equal? (car expr) var))))


(define (test1 show?)
  (cond
    (show?
     (local
       ((define expr '(+ (power x 2) 3))
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
       ((define expr '(* a y))
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
       ((define expr '(+ x x))
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
       ((define expr  '(+ x (+ x y)))
        (define var 'x))
       (begin
         (display "Test6\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test6 show?)


(define (test7 show?)
  (cond
    (show?
     (local
       ((define expr  '(+ (+ y y) (+ x x)))
        (define var 'x))
       (begin
         (display "Test7\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test7 show?)

(define (test8 show?)
  (cond
    (show?
     (local
       ((define expr '(* x x))
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
       ((define expr '(+ x 3))
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
       ((define expr '(* x y))
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
       ((define expr '(* (* x y) (+ x 3)))
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
         (newline))))
    (else (void))))

(test11 show?)

(define (test12 show?)
  (cond
    (show?
     (local
       ((define expr '(+ (+ (* a (* x x)) (* b x)) c))
        (define var 'x))
       (begin
         (display "Test12\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test12 show?)

(define (show-descr2 show?)
  (local
      ((define descr
#<<here-string

Exercise 2.56: Show how to extend the basic differ-
entiator to handle more kinds of expressions.
For instance, implement the differentiation rule

d(u^n)/dx = nu^n-1du/dx

by adding a new clause to the deriv program and
defining appropriate procedures exponentiation?,
base, exponent, and make-exponentiation. (You
may use the symbol ** to denote exponentiation.)
Build in the rules that anything raised to the power
0 is 1 and anything raised to the power 1 is the thing
itself.
here-string
         ))
    (begin
     (if show? (display descr) (void))
     (newline)
     (newline))))

(show-descr2 show?)

(define (derivative2 expr var)
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
          (sum-deriv (sum-get-u expr) (sum-get-v expr)))
         ((is-prod? expr)
          (prod-deriv (prod-get-u expr) (prod-get-v expr)))
         ((is-exp? expr)
          (exp-der (exp-get-base expr) (exp-get-n expr)))
         (else
          (begin
            (display "Error: unknown expression ")
            (display expr) (newline)
            false)))))
    (recurse expr)))

(define (is-exp? expr) (equal? (car expr) '**))
(define (exp-get-base expr) (car (cdr expr)))
(define (exp-get-n expr) (car (cdr (cdr expr))))
(define (exp-make base n)
  (cond
    ((and (number? base) (number? n)) (expt base n))
    ((equal? n 0) 1)
    ((equal? n 1) base)
    (else (list '** base n))))

(define (test13 show?)
  (cond
    (show?
     (local
       ((define expr '(+ (+ (* a (** x 2)) (* b x)) c))
        (define var 'x))
       (begin
         (display "Test13\n")
         (display "expression ") (display expr) (newline)
         (display "variable ") (display var) (newline)
         (display "result ")
         (local
           ((define res (derivative2 expr var)))
           (if (equal? res false) (void) (display res)))
         (newline)
         (newline))))
    (else (void))))

(test13 show?)


(define (descr3 show?)
  (if (not show?)
      (void)
      (local
        ((define descr
           #<<here-string
Exercise 2.57: Extend the differentiation program to
handle sums and products of arbitrary numbers of
(two or more) terms. Then the last example above
could be expressed as

(deriv ’(* x y (+ x 3)) ’x)

Try to do this by changing only the representation
for sums and products, without changing the deriv
procedure at all. For example, the addend of a sum
would be the first term, and the augend would be the
sum of the rest of the terms.
here-string
           ))
        (begin
          (display descr)
          (newline)
          (newline)))))

(descr3 show?)


;(deriv ’(* x y (+ x 3)) ’x)