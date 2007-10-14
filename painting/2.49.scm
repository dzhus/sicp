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
    (lambda (drawing)
      (if (null? segment-list)
          drawing
          (((segments->painter (cdr segment-list))
            frame)
           ((draw-segment
              (let ((segment (car segment-list)))
                (make-segment
                 ((frame-coord-map frame) (start-segment segment))
                 ((frame-coord-map frame) (end-segment segment)))))
             drawing))))))

(define stroke-border
  (segments->painter
    (make-path
     (make-vect 0 0)
     (make-vect 1 0)
     (make-vect 1 1)
     (make-vect 0 1)
     (make-vect 0 0))))

(define draw-cross
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