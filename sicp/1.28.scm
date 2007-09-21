(define (square x) (* x x))

(define (expmod base exp m)
  (define (root? x)
    (= (remainder (square x) m) 1))
    (cond
     ((= 0 exp) 1)
     ((even? exp)
      (if (= (remainder (square (expmod base (/ exp 2) m)) m) 1)
          0
          (remainder (square (expmod base (/ exp 2) m)) m)
      ))
     (else
      (remainder (* base (expmod base (- exp 1) m)) m))
     )
    )