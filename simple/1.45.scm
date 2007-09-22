(define (fixed-point f first-guess epsilon)
  (define (close-enough? a b)
    (< (abs (- a b)) epsilon))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (root-transform y n)
  (lambda (x) (/ y (expt x (- n 1)))))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (times f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (compose-step g k)
    (if (= k 1)
        g
        (compose-step (compose f g) (- k 1))))
  (compose-step f n))

(define (average-damp-times f n)
  (times (average-damp f) n))