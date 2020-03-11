;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname CointCounter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Example: Counting change
; How many ways an amount of money can be exchanged into available currency denominations

; Withoug memoization
(define (cc amount coins)
  (cond
    ((= amount 0) 1)
    ((< amount 0) 0)
    ((empty? coins) 0)
    (else (+
           (cc amount (rest coins))
           (cc (- amount (first coins)) coins)))))

(check-expect (cc 10 '(1 2 5)) 10)
(check-expect (cc 100 '(1 5 10 25 50)) 292)

; NOTE: The same algorithm can be used to solve integer equation of the form:
; c1 * x1 + ... + cn * xn = a
; where a, c1 ... cn are non-negative integers constants and x1 ... xn are variables (unknowns)


; Lookup table

;(define table (make-hash))
;(define key-a-5-coins-empty '(5 '()))
;(define key-a-5-coins-2-5 '(5 '(2 5)))
;(hash-set! table key-a-5-coins-empty 0)
;(hash-ref table key-a-5-coins-empty)
;(hash-has-key? table key-a-5-coins-2-5)
;(hash-set! table key-a-5-coins-2-5 1)
;(hash-ref table key-a-5-coins-2-5)

(define lookup-table (make-hash))

; With memoization

(define (cc-mem amount coins)
  (let
      ((key (list amount coins)))
    (cond
      ((hash-has-key? lookup-table key) (hash-ref lookup-table key))
      ((= amount 0) (let ((temp (hash-set! lookup-table key 1))) 1))
      ((< amount 0) (let ((temp (hash-set! lookup-table key 0))) 0))
      ((empty? coins) (let ((temp (hash-set! lookup-table key 0))) 0))
      (else
       (let*
           ((value
             (+
              (cc-mem amount (rest coins))
              (cc-mem (- amount (first coins)) coins)))
            (temp
             (hash-set! lookup-table key value)))
         value)))))
  
(check-expect (cc-mem 10 '(1 2 5)) 10)
(check-expect (cc-mem 100 '(1 5 10 25 50)) 292)






       