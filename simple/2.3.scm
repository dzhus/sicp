(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-segment start end) (cons start end))

(define (segment-start segment) (car segment))

(define (segment-end segment) (cdr segment))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))

(define (print-segment segment)
  (display "[")
  (print-point (segment-start segment))
  (display ", ")
  (print-point (segment-end segment))
  (display "]")
  (newline))

(define (midpoint segment)
  (let ((start (segment-start segment))
        (end (segment-end segment)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

(define (sqr x) (* x x))

(define (length segment)
  (let ((start (segment-start segment))
        (end (segment-end segment)))
    (sqrt (+ (sqr (- (x-point start) (x-point end))) (sqr (- (y-point start) (y-point end)))))))

;; Produce a vector pointing from (0, 0)
(define (vectorize segment)
  (let ((start (segment-start segment))
        (end (segment-end segment)))
    (make-segment (make-point 0 0)
                  (make-point (- (x-point end) (x-point start))
                              (- (y-point end) (y-point start))))))

;; Scalar multiplication of two vectors
(define (scalar-mult a b)
  (+ (* (x-point (segment-end a)) (x-point (segment-end b)))
     (* (y-point (segment-end a)) (y-point (segment-end b)))))

;; Produce a rectangle using two of its ortogonal sides
(define (make-rectangle a b)
  ;; Check if sides are ortogonal
  (if (and (= (scalar-mult (vectorize a) (vectorize b)) 0))
      0
      (cons a b)
      ))

(define (a-side rect) (car rect))

(define (b-side rect) (cdr rect))

;; Calculate rectangle area
(define (rect-area rect)
  (* (length (a-side rect)) (length (b-side rect))))

(define (rect-perimeter rect)
  (* (+ (length (a-side rect)) (length (b-side rect))) 2))