#lang scheme

(require "get-put.ss"
         "generic-arith.ss"
         "apply-generic.ss")

(provide raise)

(define (raise-integer n)
  (make-rational n 1))

(define (raise-rational r)
  (make-real (/ (numer r)
                (denom r))))

(define (raise-real x)
  (make-complex-from-real-imag x 0))

(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)

(define (raise x)
  (apply-generic 'raise x))