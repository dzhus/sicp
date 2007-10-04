(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-squares-of-max a b c)
  (sum-of-squares
   (if (> a b) a b)
   (cond 
    ((> c b) c)
    ((> c a) c)
    ((> a b) b)
    (else a))))