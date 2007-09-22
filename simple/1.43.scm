(define (compose f g)
  (lambda (x) (f (g x))))

(define (times-recursive f n)
  (if (= n 1)
      f
      (compose f (times-recursive f (- n 1)))))

(define (times-iterative f n)
  (define (compose-step g k)
    (if (= k 1)
        g
        (compose-step (compose f g) (- k 1))))
  (compose-step f n))
