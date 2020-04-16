#lang racket

(require rackunit)

;2.27

(define (reverse l)
  (if (null? l)
      null
      (append (reverse (cdr l))
              (list (car l)))))

(check-equal? (reverse '(1 2 5 6)) '(6 5 2 1))

(define (deep-reverse items)
  (cond
    ((null? items) null)
    ((pair? (car items)) (append (deep-reverse (cdr items))
                                 (list (deep-reverse (car items)))))
    (else (append (deep-reverse (cdr items))
                  (list (car items))))))

(check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1)))

;2.28

(define (fringe items)
  (cond
    ((null? items) null)
    ((pair? items) (append (fringe (car items))
                           (fringe (cdr items))))
    (else (list items))))

(define x (list (list 1 2) (list 3 4)))

(check-equal? (fringe x) '(1 2 3 4))

(check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4))

;2.29 - a

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (structure branch)
  (cadr branch))

(define (length branch)
  (car branch))

(define m (make-mobile (make-branch 1 4)
                       (make-branch 3 (make-mobile (make-branch 1 2)
                                                   (make-branch 1 2)))))

(check-equal? (length (left-branch m)) 1)
(check-equal? (length (right-branch m)) 3)
(check-equal? (structure (left-branch m)) 4)

;- b

(define (total-weight mobile)
  (cond
    ((pair? mobile) (+ (total-weight (structure (left-branch mobile)))
                       (total-weight (structure (right-branch mobile)))))
    (else mobile)))

(check-equal? (total-weight m) 8)

;- c

(define balanced-m (make-mobile (make-branch 1 12)
                                (make-branch 3 (make-mobile (make-branch 1 2)
                                                            (make-branch 1 2)))))

(define (torque branch)
  (cond
    ((pair? (structure branch)) (* (length branch)
                                   (+ (torque (left-branch (structure branch)))
                                      (torque (right-branch (structure branch))))))
    (else (* (length branch)
             (structure branch)))))

(define (balanced? mobile)
  (equal? (torque (left-branch mobile))
          (torque (right-branch mobile))))
 
(check-equal? (torque (left-branch balanced-m)) 12)
(check-equal? (torque (right-branch balanced-m)) 12)

(check-equal? (balanced? m) #f)
(check-equal? (balanced? balanced-m) #t)

;- d

; If the list function in the constructor was replaced by cons, the cdr function would return a number instead of a pair. The only areas which would
; need changing would be the selectors, which would need to access the cdr directly rather than car'ing with cadr. This is a design advantage of
; using selectors.


