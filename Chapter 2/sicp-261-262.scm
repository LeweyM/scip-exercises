					; 2.61

					; This procedure iterates through the list and inserts only when the element is higher than the next item in the list. This has a worst case of n, best case of 0, and average case of n/2.

(define (adjoin-set-ordered x set)
  (letrec ((iter (lambda (left x rest)
		   (cond ((null? rest)
			  (append left (list x)))
			 ((= x (car rest))
			  (append left rest))
			 ((< x (car rest))
			  (append left
				  (list x)
				  (list (car rest))
				  (cdr rest)))
			 ((> x (car rest))
			  (iter (append left (list (car rest)))
				x
				(cdr rest)))))))
    (iter '() x set)))

(adjoin-set-ordered 4 '(1 2 3 5 6)) 

					; 2.62

					; Iterating through s1, if x1 = x2, the union is x1 + union of cdr s1 and cdr s2. If x1 is lower than x2, the union is x1 + union of cdr s1 and s2. If x1 is higher than x2, the union is x2 + union of s1 and cdr s2. This iterates through s1 and s2, so the steps are o(n).
				    

(define (union-set-ordered s1 s2)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else (let ((x1 (car s1)) (x2 (car s2))) 
		(cond ((= x1 x2)
		       (cons x1 (union-set-ordered (cdr s1) (cdr s2))))
		      ((< x1 x2)
		       (cons x1 (union-set-ordered (cdr s1) s2)))
		      ((> x1 x2)
		       (cons x2 (union-set-ordered s1 (cdr s2)))))))))	 

(union-set-ordered '(1 3 5) '(0 1 4)) ;(0 1 3 4 5)

