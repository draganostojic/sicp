#lang racket

(require racket/dict)

; data-directed programming

; table is implemented as a dictionary/set of key-valye
; pairs (internally represented as an ordered list or bst)

;(define d (dict-set null 'a 123))
;
;(dict-has-key? d 'a)
;
;(dict-ref d 'a)
;
;(for ((key (dict-keys d)))
;  (begin
;    (display key) (display " ") (display (dict-ref d key)) (newline)))
;
;(define d2 (dict-set* null 'a 123 'b 34 'abc  -2))
;
;d2