(define (filter-accumulate-iterative null-value combiner filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a) 
                                            (term a)
                                            null-value)
                                 ))))
  (iter a null-value)
  )

(define (filter-accumulate-recursive null-value combiner filter term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate-recursive null-value combiner term (next a) next b)
                (if (filter a)
                    (term a)
                    null-value)))
  )

(define (self x) x)

(define (next x) (+ 1 x))

(define (all x) (= x x))

(define (filter-product filter term a next b)
  (filter-accumulate-iterative 1 * filter self a next b)
)

(define (product term a next b)
  (filter-product all self a next b))


(define (mutual-primes-product n)
  (define (mutually-primes? a)
    (= (gcd a n) 1))
  (filter-product mutually-primes? self 1 next n))
