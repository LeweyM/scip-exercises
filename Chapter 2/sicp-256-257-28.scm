(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; derivative code

(deriv '(+ x 3) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 2.56

; d(u^n)/dx = nu^(n−1)du/dx

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-exponentiation
	   (make-product (base exp) (exponent exp))
	   (make-sum (exponent exp) -1))
	  (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
	((=number? exp 1) base)
	((list '** base exp))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

;(deriv '(+ (** x 2) (* 5 x) 10) 'x)

					; 2.57

(define (make-product . m)
  (letrec ((make-product-fold 
	 (lambda (l acc symbols)
	   (cond ((eq? '() l) (append '(*) (list acc) symbols))
		 ((variable? (car l)) (make-product-fold
				       (cdr l) acc (append symbols (list (car l)))))
		 ((=number? 0 (car l)) 0)
		 (else (make-product-fold
			(cdr l) (* acc (car l)) symbols))))))
    (let ((result (make-product-fold m 1 (list))))
      (if (= (multiplier result) 1)
	  (append '(*) (cddr result))
	  result))))

(make-product 2 4 'x 1)

					; this works but requires changing the deriv implementation

(define (make-product-2 m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-product-list l)
  (if (eq? l '())
      1
      (make-product-2 (car l) (make-product-list (cdr l)))))

(define (make-product . m)
  (make-product-list m))

(make-product 'x 'y 3)

(define (make-sum . xs)
  (define (make-sum-list xs)
    (if (null? xs)
        0
        (make-sum-two-elem (car xs)
                           (make-sum-list (cdr xs)))))
  (define (make-sum-two-elem x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y)) (+ x y))
          (else (list '+ x y))))
  
  (make-sum-list xs))

(make-sum 4 5 2 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)

					;2.58

					;1

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(sum? '(1 + 2)) ;#t
(make-sum 5 (make-sum 1 4)) ;10

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))

(product? '(4 * 3)) ;#t
(make-product 2 (make-sum 1 'x)) ;'(2 * (1 + x))

(deriv '(x + (3 * ((x * 2) + (y + 2)))) 'x) ;7

					;2
; algorithm uses two stacks, one for variable and one for operators
;  an operator with higher precedence is encountered, the last two vars are reduced and the op is popped
; at the end, all remaining ops and vars are popped and reduced.


; 2 * 3 + 1 
; 2   2, 
; *   2, * 
; 3   2 3, *
; +   (2*3), +     lower
; 1   ((2*3) + 1), 
 
; 2 + 3 * 1 
; 2   2, 
; +   2, +        higher
; 3   2 3, +
; *   2 3, + *    higher
; 1   2 3 1, + * 
;     2 (3*1), +
;     (2+(3*1))

; 1 * 2 + 3 * 4 - 1
; 1    1,
; *    1, *
; 2    1 2, *
; +    (1*2), +               lower
; 3    (1*2) 3, +
; *    (1*2) 3, + *           higher
; 4    (1*2) 3 4, + *
; -    (1*2) (3*4), + -       lower
; 1    (1*2) (3*4) 1, + -
;      (1*2) ((3*4)-1), +
;      ((1*2)+((3*4)-1)),

(define (make-parens l)
  (define (precedence op)
    (cond ((eq? op '+) 1)
	  ((eq? op '-) 1)
	  ((eq? op '*) 2)))
  
  (define (operator? x)
    (or (eq? x '+)
	(eq? x '*)
	(eq? x '-)))
  
  (define (parse-by-precedence rest vals ops)
    (if (> (precedence (car rest)) (precedence (car ops)))
	;higher
	(iter (cdr rest) vals (cons (car rest) ops))
	;lower
	(iter (cdr rest) (apply-op vals (car ops)) (cons (car rest) (cdr ops))))) ;todo

  (define (apply-op vals op)
    (cons (list (cadr vals) op (car vals))
	  (cddr vals)))
  
  (define (unwind vals ops)
    (cond ((null? ops)
	   vals)
	  (else (unwind (apply-op vals (car ops)) (cdr ops) ))))

  (define (iter rest vals ops)
    (cond ((null? rest) (unwind vals ops))
	  ((operator? (car rest))
	   (if (null? ops)
	       (iter (cdr rest) vals (cons (car rest) ops))
	       (parse-by-precedence rest vals ops)))
	  ((or (number? (car rest)) (variable? (car rest)))
	   (iter (cdr rest) (cons (car rest) vals) ops))
	  ((pair? (car rest))
	   (iter (cdr rest) (cons (make-parens (car rest)) vals) ops))))

  (car (iter l '() '())))
	 
(make-parens '(1 * 2 + 3 * 4 - 1))     ; '((1 * 2) + ((3 * 4) - 1))
(make-parens '(x + 3 * (x + y + 2)))   ; '(x + (3 * ((y + x) + 2)))
(make-parens '(2 + 1 - 3 * 4 + 5 * 7)) ; '((2 + 1) - ((3 * 4) + (5 * 7)))
(make-parens '(x + (2 * y)))           ; '(x + (2 * y))

