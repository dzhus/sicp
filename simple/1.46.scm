(define (iterative-improve good? improve)
  (define (solve x)
    (if (good? x)
        x
        (solve (improve x))))
  solve)

(define (fixed-point f guess eps)
  (define (good? x)
    (< (abs (- (f x) x)) eps))
  (define (apply x)
    (f x))
  ((iterative-improve good? apply) (f guess)))

(define (sqrt y eps)
  (define (good? x)
    (< (abs (- (* x x) y)) eps))
  (define (improve x)
    (/ (+ x (/ y x)) 2))
  ((iterative-improve good? improve) (improve 1)))