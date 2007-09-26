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
;; @assert (= x (radius (make-center-percent x 100)))
(define (percent i)
  (/ (radius i) (center i)))

(define (mult-interval p q)
  (let ((p1 (* (upper-bound p) (upper-bound q)))
        (p2 (* (lower-bound p) (upper-bound q)))
        (p3 (* (upper-bound p) (lower-bound q)))
        (p4 (* (lower-bound p) (lower-bound q))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Consider following interval multiplication (we ignore signs at
;; interval radius dX and dY):
;; 
;; $(X + dX)(Y + dY) = XY + YdX + XdY + dX dY$ and as $dX \rightarrow
;; 0, dY \rightarrow 0$ it is approximately equal to $XY + YdX + XdY$.
;;
;; $X$ is interval center (median value) and $dX$ is its radius, thus
;; we may write the following procedure.
(define (mult-interval-approx p q)
  (make-center-radius (* (center p) (center q))
                      (+ (* (radius p) (center q))
                         (* (radius q) (center p)))))