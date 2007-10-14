(define (make-vect x y)
  (cons x y))

(define (x-vect vect)
  (car vect))

(define (y-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect
   (+ (x-vect vect1) (x-vect vect2))
   (+ (y-vect vect1) (y-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect
   (+ (x-vect vect1) (x-vect vect2))
   (+ (y-vect vect1) (y-vect vect2))))

(define (scale-vect vect s)
  (make-vect
   (* (x-vect vect) s)
   (* (y-vect vect) s)))
  