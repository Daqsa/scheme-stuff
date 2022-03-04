(define (average x y) 
  	(/ (+ x y) 2))

; want to approximate cbrt(x) using Newton's method
; get a slightly better approximation by taking avg(y, x/y)
(define (better-cbrt x y)
	(/  (+  (/ 	x 
			   	(square y)) 
			(* 2 y))
		3))
(define epsilon 1e-5)

(define (square x) (* x x))

; check if y is within epsilon fraction of x
(define (is-within x y)
  (< (/ (abs (- x y)) x) epsilon))


; stop when the new guess didn't change too much from old guess
(define (iterate-cbrt x y)
	(if (is-within y (better-cbrt x y))
		y
		(iterate-cbrt x 
					  (better-cbrt x y))))


