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

;; Raise argument to the next level. We assume that raising is
;; implemented for every type present in the tower but the highest
;; one; an attempt to raise element from the highest level gives
;; `'top` symbol (hence generic operations must no return this symbol
;; as a result (see another example of such limitation in a solution
;; to 2.82))
(define (raise x)
  (let* ((type (type-tag x))
         (proc (get 'raise type)))
    (if proc
        (proc x)
        'top)))
