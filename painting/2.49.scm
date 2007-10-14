(load "2.46.scm")
(load "2.47.scm")
(load "2.48.scm")

(define (frame-coord-map frame)
  (lambda (vector)
    (add-vect
     (origin frame)
     (add-vect
      (scale-vect (edge1 frame) (x-vect vector))
      (scale-vect (edge2 frame) (y-vect vector))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
       segment-list)))

(define stroke-border
  (segments->painter
    (make-path
     (make-vect 0 0)
     (make-vect 1 0)
     (make-vect 1 1)
     (make-vect 0 1)
     (make-vect 0 0))))

(define stroke-border
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 1 0)
                  (make-vect 0 1)))))

(define rhombus
  (segments->painter
   (make-path
    (make-vect 0 1/2)
    (make-vect 1/2 0)
    (make-vect 1 1/2)
    (make-vect 1/2 1)
    (make-vect 0 1/2))))