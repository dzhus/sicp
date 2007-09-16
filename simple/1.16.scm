(define (fast-expt b n a)
  (if (= n 1)
      (* b a)
      (fast-expt (square b) (quotient n 2) (if (even? n) a (* a b)))
      )
  )

(define (square x) (* x x))

(define (exp base power)
  (if (= power 0)
      1
      (fast-expt base power 1)
      )
  )