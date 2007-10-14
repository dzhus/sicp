;; This file uses global state variables (which sucks) due to the
;; nature of Cairo Guile bindings, which is due to nature of Cairo
;; itself, which is due to nature of concept of drawing.

(use-modules (cairo))

(load "2.46.scm")

(define (make-surface width height)
  (cairo-image-surface-create 'rgb24 width height))

(define ctx (cairo-create (make-surface 500 500)))

(define (prepare ctx)
  (cairo-set-source-rgb ctx 1 1 1)
  (cairo-paint ctx)
  (cairo-set-source-rgb ctx 0.5 0.5 1)
  (cairo-set-line-width ctx 1))

(prepare ctx)

(define (draw-line from to)
  (cairo-move-to ctx (x-vect from) (y-vect from))
  (cairo-line-to ctx (x-vect to) (y-vect to))
  (cairo-stroke ctx))

(define (save-drawing)
  (cairo-surface-write-to-png (cairo-get-target ctx)
                              "draw.png"))
