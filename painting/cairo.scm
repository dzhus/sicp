(use-modules (cairo))

(load "2.46.scm")

(define (make-drawing width height)
  (let ((surface (cairo-image-surface-create 'rgb24 width height)))
    (let ((ctx (cairo-create surface)))
      ;; Set up default drawing options
      (cairo-set-source-rgb ctx 1 1 1)
      (cairo-paint ctx)
      (cairo-set-source-rgb ctx 0 0 0)
      (cairo-set-line-width ctx 1)
      ctx)))

(define (draw-segment segment)
  (lambda (drawing)
    ;; I hope Guile's GC will deal with it properly
    (let ((local (cairo-create (cairo-get-target drawing)))
          (from (start-segment segment))
          (to (end-segment segment)))
      (cairo-move-to local (x-vect from) (y-vect from))
      (cairo-line-to local (x-vect to) (y-vect to))
      (cairo-stroke local)
      local)))

(define (save-drawing drawing)
  (cairo-surface-write-to-png (cairo-get-target drawing)
                              "draw.png"))
