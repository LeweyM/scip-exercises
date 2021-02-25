					; 2.73

					; 1. We cannot assimilate number and variable cases into the generic operations because they do not match the pattern of applying an operator and operand to the procedure to get a deriv. The arguments must match the generic interface.

					; 2.

(define (install-sum-package)
  ;; internal procedures
  
  (define (make-sum a1 a2) (cons a1 a2)) 
  (define (addend s) (cadr s)) 
  (define (augend s) (caddr s)) 
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  ;; interface to the rest of the system

  (define (tag x) (attach-tag '+ x))
  (put 'make-sum '+ 
       (lambda (x y) (tag (make-sum x y)))) 
  (put 'deriv '(+) deriv-sum)
  'done)

(define (install-product-package)
  ;; internal procedures
  
  (define (make-product m1 m2) (cons m1 m2)) 
  (define (multiplier p) (cadr p)) 
  (define (multiplicand p) (caddr p)) 
  (define (deriv-product p) 
    (make-sum 
     (make-product (multiplier exp) 
		   (deriv (multiplicand exp) var)) 
     (make-product (deriv (multiplier exp) var) 
		   (multiplicand exp)))) 
  ;; interface to the rest of the system

  (define (tag x) (attach-tag '* x)) 
  (put 'deriv '(*) deriv-product) 
  (put 'make-product '* 
       (lambda (x y) (tag (make-product x y)))) 
  'done)

					; 3.

(define (install-exponent-package)
  ;; internal procedures

  (define (exponentation-deriv expr var) 
    (make-product (exponent expr) 
                  (make-product  
		   (make-exponentiation (base expr) 
					(make-sum (exponent expr) -1)) 
		   (deriv (base expr) var)))) 
  (define (exponent expr) 
    (cadr expr)) 
  (define (base expr) 
    (car expr)) 
  (define (make-exponentiation base exponent) 
    (cond ((=number? exponent 0) 1) 
          ((=number? exponent 1) base) 
          ((=number? base 1) 1) 
          (else (list '** base exponent))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x)) 
  (put 'make-exponentiation '** 
       (lambda (x y) (tag (make-exponentiation x y)))) 
  (put 'deriv '** exponentiation-deriv)
  'done)

					; 4. The changes propose using the operator as the type index and 'deriv and the procedure name.

(put '+ 'deriv 'deriv-sum)

					; This means that operations can be packaged together under the same type.

(define (install-derivative-routines)
  (define (sum ops var)
    (make-sum
     (deriv (car ops) var)
     (deriv (cadr ops) var)))
  (define (product ops var)
    (make-sum
     (make-product (car ops)
                   (deriv (cadr ops) var))
     (make-product (deriv (car ops) var)
                   (cadr ops))))
  (define (exponent ops var)
    (make-product
     (make-product (cadr ops)
                   (make-exponentiation (car ops)
                                        (make-sum (cadr ops) -1)))
     (deriv (car ops) var)))
  (put '+ 'deriv sum)
  (put '* 'deriv product)
  (put '** 'deriv exponent))

					; 2.74

					; 1. In order to know how to apply procedures to the record, such as get-salary in part 2, we need to attach a tag to the record. We also need to return false in the case that the personnel-file doesn't have a record which will be useful in part 4. 

(define (get-record personnel-file employee)
  (let ((result ((get 'get-record personnel-file)
		 employee)))
    (if result
	(attach-tag result personnel-file)
	#f)))

					; 2. We can grab the tag from the record and use it to look up the procedure for getting the salary.

(define (get-salary record)
  ((get 'get-salary (tag-type record))
   record))

					; 3. 

(define (find-employee-record employee files)
  (if (null? files)
      #f
      (let ((record (get-record (car files) employee)))
	(if (false? record)
	    (find-employee-record employee (cdr files))
	    record))))

					; 4. When a new company - or division - wants to incorporate its record into the existing system, they need to use a unique tag-name and include implementations for both get-record and get-salary.

