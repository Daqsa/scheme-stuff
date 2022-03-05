; assume n > 0
(define (fib n) 

	(define (fib-iter n p q a b)
		(cond 
			((= n 0) b)
			((even? n) (fib-iter (/ n 2) (t-squared-p p q) (t-squared-q p q) a b))
			(else (fib-iter (- n 1) p q (t-a p q a b) (t-b p q a b)))
		)
	)

	(define (t-a p q a b)
		(+ (* (+ p q) a) 
		   (* q b))
	)

	(define (t-b p q a b)
		(+ (* q a)
		   (* p b))  	
	)

	(define (t-squared-p p q)
		(+ (square p)
		   (square q))
	)

	(define (t-squared-q p q)
		(+ (* 2 p q)
		   (square q))
	)

	(fib-iter n 0 1 1 0)
	
)
