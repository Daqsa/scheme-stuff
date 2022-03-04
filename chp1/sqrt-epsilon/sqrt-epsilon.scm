(define (average x y) 
  	(/ (+ x y) 2))

; want to approximate sqrt(x) using Newton's method
; get a slightly better approximation by taking avg(y, x/y)
(define (better-sqrt x y)
  	(average y (/ x y)))

(define epsilon 1e-5)

(define (square x) (* x x))

; check if y is within epsilon fraction of x
(define (is-within x y)
  (< (/ (abs (- x y)) x) epsilon))


; stop when the new guess didn't change too much from old guess
(define (iterate-sqrt x y)
	(if (is-within y (better-sqrt x y))
		y
		(iterate-sqrt x 
					  (better-sqrt x y))))


