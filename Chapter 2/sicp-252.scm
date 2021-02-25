(define (corner-split painter n)
  (if (= n 0)
      painter
        (let ((top-left (up-split painter (- n 1)))
              (bottom-right (right-split painter (- n 1)))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner)))))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four identity
			 flip-horiz 
                         flip-vert
			 rotate180)))
    (combine4 (corner-split painter n))))
