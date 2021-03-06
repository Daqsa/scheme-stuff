(define (fast-exp b n)
  	(cond ((= n 0) 1)
		((= (remainder n 2) 0) (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2)))) 
		((= (remainder n 2) 1) (* b (fast-exp b (- n 1))))
	)
)
