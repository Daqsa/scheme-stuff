; assume a >= b
(define (my-gcd a b)
  	(cond ((= b 0) a)
		  (else (my-gcd b (remainder a b)))))
