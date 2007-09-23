(define (logn x n)
  (/ (log x) (log n)))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car s)
  (logn (/ s (expt 3 (cdr s))) 2))

(define (cdr s)
  (if (odd? s)
      (logn s 3)
      (cdr (/ s 2))))