#lang racket

(require rackunit)

;2.21

(define (square-list-1 items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(check-equal? (square-list-1 '(1 2 3)) '(1 4 9))
(check-equal? (square-list-2 '(1 2 3)) '(1 4 9))

;2.22

(define (square x) (* x x))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(check-equal? (square-list-iter '(1 2 3)) '(9 4 1))

;(1 (2 3)) -> (iter (2 3) (1))
;(2 (3))   -> (iter (3) (4 1))
;(3)       -> (iter nil (9 4 1))
;()        -> (9 4 1)

; The list is returned in reverse order because the squares are iteratively cons'd onto the beginning of the new list (see above).
; This mimics the behaviour of a stack (FILO) as the first elements to be processed are successively pushed back each iteration and remain
; at the back of the list.

; reversing the cons order so that answer comes first is insufficient as - although it gives the correct order, car is being assigned to a list
; and cdr as a number. The result is a nested list.

;2.23

(define (for-each proc items)
  (if (null? items) #t
      (and (proc (car items))
           (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;2.24

;(list 1 (list 2 (list 3 4)))

; interpreter     -> '(1 (2 (3 4)))
; box and pointer -> [*, *-]-> [*, x]
;                     |         |
;                     v         v
;                    [1]       [*, *-]-> [*, x]
;                               |         |
;                               v         v
;                              [2]       [*, *-]-> [*, x]
;                                         |         |
;                                         v         v
;                                        [3]       [4]
;                     

; tree            ->              ^  (1 (2 (3 4)))
;                                1  \
;                                    ^  (2 (3 4))
;                                   2  \
;                                       ^  (3 4)
;                                      / \
;                                     3   4
