(define (smooth f dx)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (times f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (compose-step g k)
    (if (= k 1)
        g
        (compose-step (compose f g) (- k 1))))
  (compose-step f n))

(define (smooth-times f dx n)
  (times (smooth f dx) n))
