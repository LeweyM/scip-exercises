#lang racket

(require rackunit)

;2.30

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(define (square-tree items)
  (cond
    ((null? items) null)
    ((pair? items) (cons (square-tree (car items))
                         (square-tree (cdr items))))
    (else (* items items))))

(define (square-tree-map items)
  (map (lambda (tree)
        (if (pair? tree)
            (square-tree-map tree)
            (* tree tree)))
  items))

(check-equal? (square-tree t) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree-map t) '(1 (4 (9 16) 25) (36 49)))

;2.31

(define (square x) (* x x))

(define (square-tree-higher tree) (tree-map square tree))

(define (tree-map proc t)
  (map (lambda (node)
         (if (pair? node)
             (tree-map proc node)
             (proc node)))
       t))

(check-equal? (square-tree-higher t) '(1 (4 (9 16) 25) (36 49)))

;2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(check-equal? (subsets '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

; It works because the set of sets can be defined as the union between the subsets of cdr + the subsets of cdr with car. I.e. subsets of cdr -> () (3) (2) (3 2), subsets of cdr+car -> (1) (3 1) (2 1) (3 2 1).
; Next level: ss of cdr -> () (3), ss of cdr+car (2) (3 2)
; Next level: ss of cdr -> (), ss of cdr + car (3)

; The recursive definition is consistent at all levels.


