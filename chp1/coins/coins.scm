(define (count-change amount) (coin-count amount 5))

(define (coin-count amount coin-types)
  	(cond ((= coin-types 0) 0)
		  ((= amount 0) 1)
		  ((< amount 0) 0)
		  (else
				(+ 	(coin-count amount (- coin-types 1))
					(coin-count (- amount (denomination-amount coin-types)) coin-types)))))

(define (denomination-amount n)
  	(cond ((= n 5) 50)
		  ((= n 4) 25)
		  ((= n 3) 10)
		  ((= n 2) 5)
		  ((= n 1) 1)))

