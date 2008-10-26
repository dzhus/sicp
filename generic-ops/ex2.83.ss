#lang scheme

;;; Raise

(require "get-put.ss"
         "ddp-shared.ss"
         "packages.ss")

(provide raise)

;; Using `contents` to implement raising for integers and reals would
;; slightly violate the incapsulation, forcing us to make an
;; assumption that numbers are implemented using code in
;; `ddp-shared.ss`. See also notes below.

(define (raise-integer n)
  (make-rational (value n) 1))

(define (raise-rational r)
  (make-real (/ (numer r)
                (denom r))))

(define (raise-real x)
  (make-complex-from-real-imag (value x) 0))

(put 'raise 'integer raise-integer)
(put 'raise 'rational raise-rational)
(put 'raise 'real raise-real)
(put 'raise 'complex (lambda (x) 'top))

;; `raise` will obviously raise argument to the next level. We assume
;; that raising is implemented for every type present in the tower
;; with `raise` for the highest one returning symbol `'top`.
;;
;; I don't use `apply-generic` here because its semantics is applying
;; a procedure defined inside the package outside of it, while `raise`
;; applies a procedure defined outside the package.
;;
;; If we used `apply-generic`, we would have to change
;; `raise-integer`, `raise-rational` and `raise-real` to work with
;; «contents» part of our numbers. That would also make us have
;; `numer` and `denom` implemented in a way different from `real-part`
;; and `imag-part` are, introducing another semantical inconsistency
;; in our package.
;;
;; The rationale behind choosing not to use standard `apply-generic`
;; and all consequent descisions discussed above is that I don't want
;; to put raising operations _inside_ the packages and allow them to
;; learn about each other and have a pesky «IN UR PACKAGES, BREAKING
;; UR INCAPSULATION» situation.
(define (raise x)
  (let* ((type (type-tag x))
         (proc (get 'raise type)))
    (if proc
        (proc x)
        (error (format "RAISING IS NOT IMPLEMENTED FOR TYPE: ~a" type)))))
