(define (cons p q)
  (lambda (m) (m p q)))

(define (car m)
  (m (lambda (p q) p)))

(define (cdr m)
  (m (lambda (p q) q)))