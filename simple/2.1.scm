(define (sgn x) (/ x (abs x)))

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (sgn d)))
    (cons (* s (/ n g)) (* s (/ d g)))))

(define (numer n) (car n))

(define (denom n) (cdr n))

;; Multiplicative inverse
(define (mul-inv-rat n)
  (make-rat (denom n) (numer n)))

;; Additive inverse
(define (add-inv-rat n)
  (make-rat (- (numer n)) (denom n)))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b)) (* (number b) (denom b)))
            (* (denom a) (denom b))))

(define (sub-rat a b)
  (add-rat a (add-inv-rat b)))

(define (mul-rat a b)
  (make-rat (* (numer a) (numer b))
            (* (denom a) (denom b))))

(define (div-rat a b)
  (mul-rat a (mul-inv-rat b)))