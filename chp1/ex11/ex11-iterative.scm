(define (f n) 

	(define (f-iter count x0 x1 x2)
		(cond ((< n 3) n)
			  ((<= count 0) x0)
			  (else (f-iter (- count 1) (+ x0 (* 2 x1) (* 3 x2)) x0 x1))))

	(f-iter (- n 2) 2 1 0))
