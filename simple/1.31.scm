(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-recurrent term a next b)
  (if (> a b)
      1
      (* (term a) (product-recurrent term (next a) next b)))
  )

(define (self x) x)
(define (next x) (+ 1 x))

(define (factorial-iterative n)
  (product-iterative self 1 next n))

(define (factorial-recurrent n)
  (product-recurrent self 1 next n))

(define (pi n)
  (define (fraction n)
    (if (odd? n)
        (/ (+ n 1) (+ n 2))
        (/ (+ n 2) (+ n 1))))
  (* 4 (product-iterative fraction 1 next n)))