#lang scheme

;;; Raise

(require "get-put.ss"
         "ddp-shared.ss"
         "generic-arith.ss")

(provide raise)

;; Using `contents` to implement raising for integers and reals
;; probably breaks incapsulation

(define (raise-integer n)
  (make-rational (contents n) 1))

(define (raise-rational r)
  (make-real (/ (numer r)
                (denom r))))

(define (raise-real x)
  (make-complex-from-real-imag (contents x) 0))

(put 'raise 'integer raise-integer)
(put 'raise 'rational raise-rational)
(put 'raise 'real raise-real)

(define (raise x)
  (let* ((type (type-tag x))
         (proc (get 'raise type)))
    (if proc
        (proc x)
        (error (format "RAISING IS NOT IMPLEMENTED FOR TYPE: ~a" type)))))