					; 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(intersection-set '(1 2 4) '(2 3)) ; '(2)

(define (union-set set1 set2)
  (fold adjoin-set set1 set2))

(union-set (list 1 3 5) (list 3 7))

					; 2.60


					; adjoin set is fast as no element-of-set check required
(define (adjoin-set-uo x set)
  (cons x set))

					; element-of-set implementation is the same, will be slower though as will have to iter through repeated els
(define (element-of-set-uo? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-uo? x (cdr set)))))

					; union set is just the concatianation of the two sets
(define (union-set-uo set1 set2)
  (append set1 set2))

					; intesection-set implementation is the same.
(define (intersection-set-uo set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set-uo? (car set1) set2)
         (cons (car set1)
               (intersection-set-uo (cdr set1) 
                                 set2)))
        (else (intersection-set-uo (cdr set1) 
				   set2))))

					;                    |   ordered  | unordered
					; element-of-set     |      n     |    n
					; adjoin-set         |      n     |    0
					; intersection-set   |      n^2   |    n^2
					; union-set          |      n^2   |    0

					; unordered sets are generally much faster as no checks over the list are required, however memory use is higher and the lists can be expected to be longer (hence a higher n) if the program inserts many repeated elements. 
   
