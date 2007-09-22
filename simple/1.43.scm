(define (compose f g)
  (lambda (x) (f (g x))))

(define (times f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (times f (- n 1)))))