(define (N i) 1)

(define (D i)
  (cond ((= (remainder i 3) 2)
         (* (/ (+ i 1) 3) 2))
        (else 1)))         

(define (cont-fract N D k)
  (define (frac i result)
    (if (= i 0)
        result
        (frac (- i 1) (/ (N i) (+ (D i) result)))))
  (frac k 0))

; e
; (+ 2 (cont-fract-iterative N D 20))

; 1.0
; (log (+ 2 (cont-fract-iterative N D 20)))