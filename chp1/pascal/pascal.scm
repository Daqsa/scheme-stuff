; Computes nth row, mth column of pascal's triangle
; zero indexing
(define (pascal n m)
  (if (or (= m 0) (= m n)) 
		  1
		  (+ (pascal (- n 1) (- m 1)) (pascal (- n 1) m))))
