(define (accumulate-iterative null-value combiner term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value)
)

(define (accumulate-recursive null-value combiner term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate-recursive null-value combiner term (next a) next b) (term a)))
)

(define (self x) x)

(define (next x) (+ 1 x))

(define (sum term a next b)
  (accumulate-recursive 0 + self a next b)
)

(define (product term a next b)
  (accumulate-iterative 1 * self a next b)
)