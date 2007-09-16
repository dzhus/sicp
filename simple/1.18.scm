(define (halve x) (quotient x))

(define (double x) (+ x x))

(define (fast-mult a b k)
         (if (= b 1)
             (+ a k)
             (fast-mult (double a) (halve b) (+ k (if (odd? b) a 0)))))

(define (* a b)
  (if (or (= a 0) (= b 0))
      0
      (fast-mult a b 0))
  )
