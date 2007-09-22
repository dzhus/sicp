(define (fixed-point f first-guess epsilon)
  (define (close-enough? a b)
    (< (abs (- a b)) epsilon))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

; x^x = 1000
(define (f x)
  (/ (log 1000) (log x)))

(define (f-damped x)
  (/ (+ x (/ (log 1000) (log x))) 2))
