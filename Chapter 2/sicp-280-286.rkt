; 2.81

; a) 'apply-generic' always calls itself and therefore enters an infinite recursive loop.

; b) No, because in the case of both args being a recognised combination in the table, for example '(scheme-number scheme-number), a proc would have been found and therefore the cooersion would not take place.

; c)

(let ((type-tags (map type-tag args))) 
  (let ((proc (get op type-tags))) 
    (if proc 
      (apply proc (map contents args)) 
      (if (= (length args) 2) 
        (let ((type1 (car type-tags)) 
              (type2 (cadr type-tags)) 
              (a1 (car args)) 
              (a2 (cadr args))) 
          (if (equal? type1 type2) 
            (error "no method found") 
            (let ((t1->t2 (get-coercion type1 type2)) 
                  (t2->t1 (get-coercion type2 type1)) 
                  (a1 (car args)) 
                  (a2 (cadr args))) 
              (cond (t1->t2 
                      (apply-generic op (t1->t2 a1) a2)) 
                    (t2->t1 
                      (apply-generic op a1 (t2->t1 a2))) 
                    (else (error "no method found")))))) 
        (error "no method found")))))

; 2.82

(define (type-tag a) a) ; mock type-tag function

(define (contents a) a) ; mock contents function

(define (get-op op type-tags) ; mock get-op function
  (cond
    [(equal? type-tags '(B B B)) (lambda (a b c) (print 'result: a b c))]
    [else #false]))

(define (get-coercion arg1 arg2) ; mock-get-coercion function
  (cond 
    [(and (equal? arg1 'A) (equal? arg2 'B)) (lambda (a) 'B)]
    [else #false]))

(define (get a b)
  (if (equal? a b)
    (lambda (a) a)
    (get-coercion a b)))

(define (none-false? l)
  (foldl (lambda (a b) (and a (not (not b)))) #true l))

(define (coerce args)
  (define (iter rest)
    (if (null? rest)
      #false
      (let ((target-type (car rest))
            (coercers (map (lambda (a) (get a (car rest)))
                           args)))
        (if (none-false? coercers)
          (map (lambda (a fn) (fn a))
               args
               coercers)
          (iter (cdr rest))))))
  (iter args))

(define (apply-generic op . args) 
 (let ((type-tags (map type-tag args))) 
   (let ((proc (get-op op type-tags))) 
     (if proc 
       (apply proc (map contents args)) 
       (let ((coerced-args (coerce args))) 
         (if coerced-args 
           (let ((coerced-type-tags (map type-tag coerced-args))) 
             (let ((new-proc (get op coerced-type-tags))) 
               (apply new-proc (map contents coerced-args)))) 
           (error "No method for these types" 
                  (list op type-tags))))))))

(apply-generic 'test 'A 'A 'B 'B)

; 2.83

(define (raise x)  (apply-generic 'raise x)) 

; add into scheme-number package 
(put 'raise 'integer  
     (lambda (x)  (make-rational x 1))) 

; add into rational package 
(put 'raise 'rational 
     (lambda (x)  (make-real (/ (numer x)  (denom x))))) 

; add into real package 
(put 'raise 'real 
     (lambda (x)  (make-from-real-imag x 0))) 

; 2.84

(define (raise-to a b)
  (let ((a-type (type-tag a)) 
        (b-type (type-tag b))) 
    (cond ((equal? a-type b-type) a) 
          ((get 'raise (list a-type))  
           (raise-to ((get 'raise (list a-type))  (contents a)) b)) 
          (else #f))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if ((= 2 (length args)))
          (let ((a (car args))
                (b (cdr args)))
            (cond 
              [(raise-to a b) (apply-generic op (raise-to a b) b)]
              [(raise-to b a) (apply-generic op (raise-to b a) a)]
              [else (error "no raises possible")]))
          (error "only 2 args considered"))))))

; 2.85

; add into rational package 
(put 'project 'rational 
     (lambda (x)  (make-scheme-number (round (/ (numer x)  (denom x)))))) 

; add into real package 
(put 'project 'real 
     (lambda (x)  
       (let ((rat (rationalize  
                    (inexact->exact x) 1/100))) 
         (make-rational 
           (numerator rat) 
           (denominator rat))))) 

; add into complex package 
(put 'project 'complex 
     (lambda (x)  (make-real (real-part x)))) 








