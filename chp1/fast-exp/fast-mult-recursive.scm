(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (mult a b)
  	(cond ((= b 0) 0)
		  ((even? b) (mult (double a) (halve b)))
		  (else (+ a (* a (- b 1))))))
