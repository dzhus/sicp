(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound p) (car p))

(define (upper-bound p) (cdr p))

(define (add-interval p q)
  (make-interval (+ (lower-bound p) (lower-bound q))
                 (+ (upper-bound p) (upper-bound q))))

(define (mult-interval p q)
  (let ((p1 (* (upper-bound p) (upper-bound q)))
        (p2 (* (lower-bound p) (upper-bound q)))
        (p3 (* (upper-bound p) (lower-bound q)))
        (p4 (* (lower-bound p) (lower-bound q))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval p q)
  (define (inv-interval p)
    (make-interval
     (/ 1.0 (upper-bound p))
     (/ 1.0 (lower-bound p))))
  (mult-interval p (inv-interval q)))

(define (sub-interval p q)
  (make-interval (- (lower-bound p) (lower-bound q))
                 (- (upper-bound p) (upper-bound q))))