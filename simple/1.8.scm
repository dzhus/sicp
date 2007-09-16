(define (cbrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (cbrt-iter (improve guess x) guess x)))
(define (cbrt x) (cbrt-iter 1.0 0 x))

(define (good-enough? guess prev-guess) (< (abs (- (/ prev-guess guess) 1)) 0.01))

(define (sqr x) (* x x))
(define (improve guess x)
         (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))