#lang scheme

;;; Packages for standard types, see 2.5.1

(require "get-put.ss"
         "ddp-shared.ss"
         "apply-generic.ss"
         "complex.ss")

(provide value
         numer denom
         make-integer
         make-rational
         make-real
         make-complex-from-real-imag
         make-complex-from-mag-ang)

;; Changes from version in the book:
;;
;; - Our ierarchy is integers, rationals, reals and complex numbers
;;   (no «scheme-number» type)
;;
;; - `=zero?` predicate implemented in every package (see `ex2.80.ss`)
;;
;; - `value` procedure for integers and reals (see `ex2.83.ss`)
;;
;; - `numer` and `denom` selectors for rational numbers (needed to
;;   implement raising)


;;; Integer numbers

;; Implementation is taken from scheme-number-package: we don't want
;; to take two numerical towers into account. Solution of 2.78 isn't
;; used, too.
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer tag)
  (put 'value '(integer) (lambda (x) x))
  'done)

(install-integer-package)

(define make-integer
  (get 'make 'integer))


;;; Rational numbers
(define (install-rational-package)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define make-rational
  (get 'make 'rational))

(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))


;;; Real numbers

;; A copy&paste from integers actually (except for division), probably
;; macro needed
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'make 'real tag)
  (put 'value '(real) (lambda (x) x))
  'done)

(install-real-package)

(define make-real
  (get 'make 'real))


;;; Complex numbers
(define (install-complex-package)
  (define make-from-real-imag 
    (get 'make-from-real-imag 'rectangular))
  (define make-from-mag-ang
    (get 'make-from-mag-ang 'polar))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put '=zero? '(complex)
       (lambda (x) (< (magnitude x) 1e-15)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define make-complex-from-real-imag
  (get 'make-from-real-imag 'complex))

(define make-complex-from-mag-ang
  (get 'make-from-mag-ang 'complex))

;; 2.77
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; A `contents` wrapper for integers and reals (see `ex2.83.ss`)
(define (value x) (apply-generic 'value x))
