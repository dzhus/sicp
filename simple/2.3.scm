(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (equal-points? a b)
  (and (= (x-point a) (x-point b))
       (= (y-point a) (y-point b))))

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

;; Rectangles presented by two ortogonal sides
(define (make-rect-ort a b)
  ;; Check if sides are ortogonal
  ;; Check whether given segments cross at ends is *not* performed
  (if (and (= (scalar-mult (vectorize a) (vectorize b)) 0))
      0
      (cons a b)))

(define (a-side-ort rect) (car rect))

(define (b-side-ort rect) (cdr rect))

;; Rectangles presented by its diagonals
(define (make-rect-diag m n)
  ;; Diagonals of a rectangle are equal and cross at midpoints
  (if (and (= (length m) (length n))
           (equal-points? (midpoint m) (midpoint n)))
      (cons m n)
      0))

(define (a-diagonal-diag rect) (car rect))

(define (b-diagonal-diag rect) (cdr rect))

(define (a-side-diag rect)
  (let ((a (segment-start (a-diagonal-diag rect)))
        (b (segment-start (b-diagonal-diag rect))))
  (make-segment (make-point (x-point a) (y-point a))
                (make-point (x-point b) (y-point b)))))

(define (b-side-diag rect)
  (let ((a (segment-start (a-diagonal-diag rect)))
        (b (segment-end (b-diagonal-diag rect))))
  (make-segment (make-point (x-point a) (y-point a))
                (make-point (x-point b) (y-point b)))))

;;; Implementation notes:
;;
;; Abstraction levels (each level needs to know only about the upper one):
;; 
;; 1. Rectangles presented by two sides:
;;
;;     cons, car, cdr
;;     make-rect-ort | a-side-ort, b-side-ort
;;     make-rect     | a-side, b-side
;;                   | rect-area, rect-perimeter
;;
;; 2. Rectangles presented by two diagonals:
;;
;;     cons, car, cdr
;;     make-rect-diag | a-diagonal-diag, b-diagonal-diag
;;                    | a-side-diag, b-side-diag
;;     make-rect      | a-side, b-side
;;                    | rect-area, rect-perimeter
;;
;; Notes:
;;
;; 1. Presentation using two diagonals is allows to easily check
;;    correctness of provided data (rectangle diagonals cross at
;;    midpoints and have equal length).
;;    
;; 2. `a-diagonal-ort` and `b-diagonal-ort` are not present at all;
;;    their implementation, though, would imply the need to normalize
;;    rectangle side directions (vectorize them) or perform other
;;    checks
;;
;; 3. Higher level procedures `rect-area` and `rect-perimeter` do not
;;    care to know how rectangles are presented.

;; Select implementation
;(define a-side a-side-ort)
;(define b-side b-side-ort)
;(define make-rect make-rect-ort)
(define a-side a-side-diag)
(define b-side b-side-diag)
(define make-rect make-rect-diag)

;; Top level procedures
;; Calculate rectangle area
(define (rect-area rect)
  (* (length (a-side rect)) (length (b-side rect))))

(define (rect-perimeter rect)
  (* (+ (length (a-side rect)) (length (b-side rect))) 2))