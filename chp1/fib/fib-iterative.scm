(define (fib n)

	(define (fib-iter n a b)
		(if (= n 1)
			a
			(fib-iter (- n 1) (+ a b) a)))

	(fib-iter n 1 0))


