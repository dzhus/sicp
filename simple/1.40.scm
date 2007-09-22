(define (deriv f)
  (let ((dx 0.001))
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))

(define (fixed-point f first-guess epsilon)
  (define (close-enough? a b)
    (< (abs (- a b)) epsilon))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (root-newton f guess epsilon)
  (define (newton-transform f)
    (lambda (x) (- x (/ (f x) 
                        ((deriv f) x)))))
  (fixed-point (newton-transform f) guess epsilon))

(define (cubic a b c d)
  (lambda (x) (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d)))