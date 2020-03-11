#lang racket


(define show? true)

(define (problem-descr show?)
  (local
    ((define descr
#<<here-string

Exercise 2.42: The “eight-queens puzzle” asks how to place eight queens
on a chessboard so that no queen is in check from any other
(i.e., no two queens are in the same row, column, or diagonal). 


here-string
       ))
    (if show? (display descr) (void))))

(problem-descr show?)

(define (show-queens solutions)
  (if (null? solutions)    
      (begin
        (display "No solutions\n"))
      (begin
        (display "Each solution is a list of row col lists\n")
        (foldl (λ (solution acc)
                 (begin
                   (display "[") (display acc) (display "]: ") (display solution) (newline)
                   (+ acc 1)))
               1
               solutions)
        (void))))

(define (queens all-queens)
  (local
    ((define log? false)
     ;(define rows (range 1 9))
     (define rows (range 1 (+ all-queens 1)))
     (define initial-solutions (map (λ (row) (list (list 1 row))) rows))
     (define and-op (λ (x acc) (and x acc)))
     (define or-op (λ (x acc) (or x acc)))
     (define (safe? pos)
       (local
         ((define first-queen-pos (car pos))
          (define (row qeen-pos) (car (cdr qeen-pos)))
          (define (col queen-pos) (car queen-pos))
          (define row-first-queen (row first-queen-pos))
          (define col-first-queen (col first-queen-pos))
          (define rest-queen-posts (cdr pos))
          (define on-same-diag?
            (foldr or-op false (map (λ (rest-queen-pos)
                                      (= (abs (- col-first-queen (col rest-queen-pos)))
                                         (abs (- row-first-queen (row rest-queen-pos)))))
                                    rest-queen-posts)))
          (define on-same-row?
            (foldr or-op false (map (λ (rest-queen-pos)
                                      (= row-first-queen (row rest-queen-pos)))
                                    rest-queen-posts))))
         (and (not on-same-row?) (not on-same-diag?))))       
     (define (iterate placed-queens prev-sols)
       (cond
         ((= placed-queens all-queens) prev-sols)
         (else
          (local
            ((define queen (+ placed-queens 1))
             (define new-posts (map (λ (row) (list queen row)) rows))
             (define all-posts
               (foldr append
                      null
                      (map (λ (prev-sol)
                             (map (λ (new-pos)
                                    (append (list new-pos) prev-sol))
                                  new-posts))
                           prev-sols)))
             (define safe-posts (filter safe? all-posts)))
            (begin
              (if log?
                  (begin
                    (display "solutions for ") (display placed-queens) (display " queens\n")
                    (show-queens prev-sols)
                    (display "all positions for ") (display queen) (display " queens\n")
                    (show-queens all-posts)
                    (display "safe positions for ") (display queen) (display " queens\n")
                    (show-queens safe-posts)
                    (void))
                  (void))
              (iterate (+ placed-queens 1) safe-posts))))))
     (define result (iterate 1 initial-solutions)))
    (begin
      (if log?
          (begin
            (display "\n\n\nfinal result for ") (display all-queens) (display " queens\n")
            (show-queens result))
          (void))
      result)))

(define (test1 show?)
  (if show?
      (local
        ((define n-queens 2)
         (define res (queens n-queens)))
        (begin
          (display "\n\nTest1\n")
          (display "Number of queens: ") (display n-queens) (newline)
          (show-queens res)))
      (void)))

(test1 show?)

(define (test2 show?)
  (if show?
      (local
        ((define n-queens 3)
         (define res (queens n-queens)))
        (begin
          (display "\n\nTest2\n")
          (display "Number of queens: ") (display n-queens) (newline)
          (show-queens res)))
      (void)))

(test2 show?)

(define (test3 show?)
  (if show?
      (local
        ((define n-queens 4)
         (define res (queens n-queens)))
        (begin
          (display "\n\nTest3\n")
          (display "Number of queens: ") (display n-queens) (newline)
          (show-queens res)))
      (void)))

(test3 show?)

(define (test4 show?)
  (if show?
      (local
        ((define n-queens 8)
         (define res (queens n-queens)))
        (begin
          (display "\n\nTest4\n")
          (display "Number of queens: ") (display n-queens) (newline)
          (show-queens res)))
      (void)))

(test4 show?)


(define (test5 show?)
  (if show?
      (local
        ((define n-queens 10)
         (define res (queens n-queens)))
        (begin
          (display "\n\nTest5\n")
          (display "Number of queens: ") (display n-queens) (newline)
          (show-queens res)))
      (void)))

(test5 show?)

           

