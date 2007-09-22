(define (tan-cf k x)
  (define (N x) (- (* x x)))
  
  (define (D i)
    (- (* 2 i) 1))
  
  (define (frac i result)
    (if (= i 0)
        result
        (frac (- i 1) (/ (N x) (+ (D i) result)))))
  
  (- (/ (frac k 0) x)))

(define (tan-frac x)
  (tan-cf 100 x))