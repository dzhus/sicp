(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (good-enough? guess prev-guess) (< (abs (- (/ prev-guess guess) 1)) 0.01))
(define (sqrt x) (sqrt-iter 1.0 0.0 x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))