;; Basic constructor
(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound p) (car p))

(define (upper-bound p) (cdr p))

;; Define interval using median value and radius
(define (make-center-radius c r)
  (make-interval (- c r)
                 (+ c r)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; @assert (= x (radius (make-center-radius c x)))
(define (radius i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;; Define interval using median value and percentage of radius to
;; median value
(define (make-center-percent c e)
  (make-center-radius c (abs (* c (/ e 100)))))

;; @assert (= x (percent (make-center-percent c x)))
(define (percent i)
  (/ (radius i) (center i)))