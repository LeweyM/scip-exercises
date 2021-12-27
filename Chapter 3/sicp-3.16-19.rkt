(require sicp)

; 3.16

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define l1 
  (list 1 2 3))
; (1->(2->(3->NULL)))

(define l2
  (let ([x (list 'a)])
    (list x x)))
; (a->LOOP TO a) 

(define l3
  (let* ([x (list 'a)]
         [y (cons x x)])
    (cons y y)))

(define (last-pair x)
  (if (null? (mcdr x))
    x
    (last-pair (mcdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define l4 (make-cycle (list 'a 'b 'c)))

(count-pairs l1) ;3
(count-pairs l2) ;4
(count-pairs l3) ;7
; (count-pairs l4) ;infinite loop

; 3.17

(define (any? pred l)
  (if (null? l) 
    #false 
    (let ((el (car l))
          (rest (cdr l)))
      (if (pred el) 
        #true
        (any? pred rest)))))

(define (count-pairs x) 
  (let ((encountered '())) 
    (define (helper x) 
      (if (or (not (pair? x)) (any? (lambda (y) (eq? x y)) encountered)) 
        0 
        (begin 
          (set! encountered (cons x encountered)) 
          (+ (helper (car x)) 
             (helper (cdr x)) 
             1)))) 
    (helper x))) 

(count-pairs l1) ;3
(count-pairs l2) ;3
(count-pairs l3) ;3

; 3.18

(define (has-cycle? l)
  (let ((encountered '()))
    (define (helper x)
      (cond 
        ((null? x) #f)
        ((memq x encountered) #t)
        (else 
          (begin 
            (set! encountered (cons x encountered))
            (helper (cdr x))))))
    (helper l)))

(has-cycle? l4) ;#t
(has-cycle? '(1 2 3)) ;#f

; 3.19

; This is the two runner trick used for detecting cycles in linked lists. Send one through the list, and another through the list at twice the speed. If one catches up to the other, the list has a cycle. If the fast runner finishes the list, there is no cycle.

(define (has-cycle-nomem? l)
  (let ((slow l)
        (fast (cddr l)))
    (define (helper x)
      (if (null? x) #f
        (cond 
          ((or (null? slow) (null? fast)) #f)
          ((null? (cdr fast)) #f)
          ((eq? slow fast) #t)
          (else (begin 
                  (set! slow (cdr slow))
                  (set! fast (cddr fast))
                  (helper (cdr x)))))))
    (helper l)))

(has-cycle-nomem? l4); #t
(has-cycle-nomem? '(1 2 3 4)); #f
(has-cycle-nomem? '(1 2 3)); #f

; note: this will only work for lists with a minimum of 2 elements
