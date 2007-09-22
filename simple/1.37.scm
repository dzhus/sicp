(define (cont-fract N D k)
  (define (frac i)
    (if (= i k)
        (/ (N i) (D i))
        (/ (N i) (+ (D i)
                    (frac (+ i 1))))
        ))
  (frac 1))

