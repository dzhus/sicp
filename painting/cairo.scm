;; This code implements somewhat functional approach to drawing:
;;
;; Create picture with `(define pic (make-drawing 500 500))`. Get a
;; new picture with something drawn on it using `((draw-segment
;; segment) pic)`
;;
;; 2.49.scm (an excercise from SICP) has a more interesting
;; `segments->painter` procedure and introduces concept of frames as
;; picture transformations. With 2.49 loaded, you may do things as
;; funky as the following: 
;;
;;     (save-picture ((rhombus (make-frame (make-vect 25 25)
;;                                         (make-vect 450 0)
;;                                         (make-vect 0 450)))
;;                    (make-picture 500 500)))
;;


(use-modules (cairo))

(load "2.46.scm")

(define (make-picture width height)
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

(define (save-picture picture)
  (cairo-surface-write-to-png (cairo-get-target picture)
                              "draw.png"))
