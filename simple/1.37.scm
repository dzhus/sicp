(define (cont-fract-recursive N D k)
  (define (frac i)
    (if (= i k)
        (/ (N i) (D i))
        (/ (N i) (+ (D i)
                    (frac (+ i 1))))
        ))
  (frac 1))

(define (cont-fract-iterative N D k)
  (define (frac i result)
    (if (= i 0)
        result
        (frac (- i 1) (/ (N i) (+ (D i) result)))))
  (frac k 0))

