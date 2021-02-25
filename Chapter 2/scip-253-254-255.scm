					;2.53

(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2)) 
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoes) (blue socks))) ; false 
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)

					;2.54

(define (my-equal? a b)
  (if (pair? a)
      (and (pair? b)
	   (my-equal? (car a) (car b))
	   (my-equal? (cdr a) (cdr b)))
      (eq? a b)))

(my-equal? '(this is a list) 
	   '(this (is a) list))

					;2.55
(car ''abracadabra)

; The following expressions are equivalent

''abracadabra

(quote abracadabra)

(list 'quote 'abracadabra)

; Expressed as symbols in a list, it's clear that the car of (list 'quote 'abracadabra) will return the first element, 'quote.
