(define (mult x y)

	(define (double x) (* x 2))
	(define (halve x) (/ x 2))

	(define (mult-iter a b product)
		(cond ((= b 0) product)
			  ((even? b) (mult-iter (double a) (halve b) product))
			  (else (mult-iter a (- b 1) (+ a product)))
		)
	)

	(mult-iter x y 0)
)
