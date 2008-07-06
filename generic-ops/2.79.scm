;;; Time paradox
(load "2.80.scm")

(define (equ? x y)
  (=zero? (apply-generic 'sub x y)))