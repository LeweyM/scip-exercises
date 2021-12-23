; 2.77

;The 'complex' package does not know how to process the operation 'magnitude'. The 'Rectangle' package, on the other hand, does.

;In order to get the 'magnitude' functionality in the 'complex' package, we have to _pass down_ the call responsiblity to the next package in the chain.

(put 'magnitude '(complex) magnitude)

;The reason that this works is because of how `apply-generic` works. When it finally makes the call to the table function, it applies the content to the operation.

(apply proc (map contents arg))

;here content _unwraps_ the complex object and passes the call responsibility to the next data type in the chain - in this case the 'Rectangle' object, which does know how to process a call to 'magnitude'.

; 2.78

(define (type-tag datum)
  (if (number? datum)
    'scheme-number
    (if (pair? datum)
      (car datum)
      (error "bad tagged datum -- TYPE-TAG"))))
(define (contents datum)
  (if (number? datum) 
    datum
    ((if (pair? datum)
       (cdr datum)                  
       (error "bad tagged datum -- CONTENT")))))
(define (attach-tag type-tag content)
  (if (number? content) 
    content
    (cons type-tag content)))

; 2.79

; interface

(define (equ a b) (apply-generic 'equ a b))

; add this to the scheme number package

(put 'equ '(scheme-number scheme-number) = )

; add this to the rational number package

(define (equ-rat a b)
  (= (* (numer a) (denom b)
        (* (numer b) (denom a)))))

(put 'equ '(rational rational) 
     (lambda (x y) (tag (equ-rat x y))))

; add this to the complex number package

(define (equ-rat a b)
  (and (= (real-part a) (real-part b)
          (= (imag-part b) (imag-part a)))))

(put 'equ '(complex complex) 
     (lambda (x y) (tag (equ-rat x y))))

; 2.80

; interface 
(define (=zero? x)  (apply-generic '=zero? x)) 
  
 ; add this to scheme-number-package 
 (put '=zero? 'scheme-number (lambda (x)  (= x 0))) 
  
 ; add this to rational-number-package 
 (put '=zero? 'rational-number  
                (lambda (x)  (= (numer x) 0))) 
  
 ; add this to complex-number-package 
 (put '=zero? 'complex-number 
                (lambda (x)  (= (real-part x)  (imag-part x) 0))) 
