#lang racket

;; Subterm reduction order detector
(define f
  (let ((state 'nop))
    (lambda (n)
      (if (eq? state 'nop)
          (begin (set! state n)
                 state)
          state))))
