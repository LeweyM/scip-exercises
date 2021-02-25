
					; 2.66

(define tree '(5 (3 (1 '() '())
		     '())
		  (9 (7 '() '())
		     (11 '() '()))))

(define record-tree '((5 bobo)
		      ((3 lau) () ())
		      ((9 lulu) () ())))

(define entry car)
(define left-tree cadr)
(define right-tree caddr)
(define key car)
(define rec cadr)

(define (lookup-tree tree given-key)
  (cond ((null? tree)
	 false)
	((= (key (entry tree)) given-key)
	 (rec (entry tree)))
	((> given-key (key (entry tree)))
	 (lookup-tree (right-tree tree) given-key))
	(else
	 (lookup-tree (left-tree tree) given-key))))

(lookup-tree record-tree 10) ;f
(lookup-tree record-tree 3)  ;lau
